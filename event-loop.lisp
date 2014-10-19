(in-package :cl-async)

(defun event-handler (errno event-cb &key socket)
  "Called when an event (error, mainly) occurs."
  (let* ((event nil)
         (errstr "TODO: grab error strings from libuv"))
    (catch-app-errors event-cb
      (unwind-protect
        (cond
          ((= errno uv:+uv--etimedout+)
           (setf event (make-instance 'tcp-timeout :socket socket :code errno :msg "connection timed out")))
          ((= errno uv:+uv--econnreset+)
           (setf event (make-instance 'tcp-reset :socket socket :code errno :msg "connection reset")))
          ((= errno uv:+uv--econnrefused+)
           (setf event (make-instance 'tcp-refused :socket socket :code errno :msg "connection refused")))
          ((= errno uv:+uv--eof+)
           (setf event (make-instance 'tcp-eof :socket socket)))
          ((= errno uv:+uv--efault+)
           (setf event (make-instance 'event-error :code errno :msg "bad address in system call argument")))
          (t
           (setf event (make-instance 'event-error :code errno :msg errstr))))
        (when event
          (unwind-protect
            (when event-cb (run-event-cb event-cb event))
            ;; if the app closed the socket in the event cb (perfectly fine),
            ;; make sure we don't trigger an error trying to close it again.
            (handler-case (and socket (close-socket socket))
              (socket-closed () nil))))))))

(defun enable-threading-support ()
  "Enable threading support in libevent. This attempts to guess which threading
   function of libevent's to use and call it.

   Note that you MUST call this *before* starting your event loop."
   (setf *enable-threading* t))

(defmacro with-threading-context ((&key base-id (io t)) &body body)
  "Use this to wrap the top level of any non-event-loop thread that is meant to
   access the event loop:
   
   (bt:make-thread
     (lambda ()
       (with-threading-context ()
         (as:with-delay (3) (print 'hello-world)))))
   
   This macro sets up buffers as well as binds the cl-async-base:*event-base*
   variable so any calls to event loop functions from your thread will have
   access to the event loop.

   If you have more than one event loop running, the ID of the event base you
   wish to operate on should be passed in:
   
   (defvar *base-id* nil)
   (with-event-loop ()
     (setf *base-id* (cl-async-base:event-base-id cl-async-base:*event-base*))
     (bt:make-thread
       (lambda ()
         (with-threading-context (*base-id*)
           ...))))
   
   You *MUST* call (enable-threading-support) before starting your event loop."
  (let ((base-id-bind (gensym "base-id"))
        (base (gensym "base")))
    `(let* ((,base-id-bind ,base-id)
            (,base (bt:with-lock-held (*event-base-registry-lock*) 
                     (if ,base-id-bind
                         ;; grab our event base by id
                         (gethash ,base-id-bind *event-base-registry*)
                         ;; grab the first event base
                         (let ((base nil))
                           (loop for val being the hash-values of *event-base-registry* do
                             (setf base val)
                             (return))
                           base))))
            (cl-async-base:*event-base* ,base)
            ;; recreate our buffers because it would be a sin to let them
            ;; intermingle with the event loop's
            (*socket-buffer-c* ,(when io `(cffi:foreign-alloc :unsigned-char :count *buffer-size*)))
            (*socket-buffer-lisp* ,(when io `(make-array *buffer-size* :element-type '(unsigned-byte 8)))))
       (unwind-protect
         (progn ,@body)
         ,(when io
            `(progn
               (cffi:foreign-free *socket-buffer-c*)
               (setf *socket-buffer-lisp* nil)))))))

(defun add-event-loop-exit-callback (fn)
  "Add a function to be run when the event loop exits."
  (push fn (event-base-exit-functions *event-base*)))

(defun process-event-loop-exit-callbacks ()
  "run and clear out all event loop exit functions."
  (dolist (fn (event-base-exit-functions *event-base*))
    (funcall fn))
  (setf (event-base-exit-functions *event-base*) nil))

(defun check-event-loop-running ()
  (unless (and *event-base* (event-base-c *event-base*))
    (error "Event loop not running. Start with function start-event-loop.")))

(defun stats ()
  "Return statistics about the current event loop."
  (list :open-dns-queries (event-base-dns-ref-count *event-base*)
        :fn-registry-count (if (hash-table-p (event-base-function-registry *event-base*))
                               (hash-table-count (event-base-function-registry *event-base*))
                               0)
        :data-registry-count (if (hash-table-p (event-base-data-registry *event-base*))
                                 (hash-table-count (event-base-data-registry *event-base*))
                                 0)
        :incoming-tcp-connections (event-base-num-connections-in *event-base*)
        :outgoing-tcp-connections (event-base-num-connections-out *event-base*)))

(defun dump-event-loop-status (file &key (return-as-string t))
  "Dump the status of the event loop to a file. Good for debugging.
   
   If return-as-string is T, the file is read/deleted and the contents returned
   as a string. Note that this is the default behavior."
  ;; TODO: see uv:uv-walk
  (error "Not implemented"))

(defvar *event-base-registry* (make-hash-table :test 'eq)
  "Holds ID -> event-base lookups for every active event loop. Mainly used when
   grabbing the threading context for a particular event loop.")

(defvar *event-base-registry-lock* (bt:make-lock)
  "Locks the event-base registry.")

(defun start-event-loop (start-fn &key fatal-cb logger-cb default-event-cb (catch-app-errors nil catch-app-errors-supplied-p))
  "Simple wrapper function that starts an event loop which runs the given
   callback, most likely to init your server/client.

   Supports setting up a callback for fatal errors. In case you don't want
   libevent to just exit your app for you.

   Supports setting up a logging callback for your application.

   *PLEASE NOTE* Using libevent functions from within the logging callback can
   lead to strange bugs and problems. Don't do it."
  (when *event-base*
    (error "Event loop already started. Please wait for it to exit."))
  ;; note the binding of these variable via (let), which means they are thread-
  ;; local... so this function can be called in different threads, and the bound
  ;; variables won't interfere with each other.
  (cffi:with-foreign-object (loop '(:pointer (:struct uv:uv-loop-s)))
    (uv:uv-loop-init loop)
    (let ((*event-base* (apply #'make-instance
                               (append
                                 (list 'event-base
                                       :c loop
                                       :id *event-base-next-id*)
                                 (when catch-app-errors-supplied-p
                                   (list :catch-app-errors catch-app-errors))
                                 (when (functionp default-event-cb)
                                   (list :default-event-handler default-event-cb)))))
          (*socket-buffer-c* (cffi:foreign-alloc :unsigned-char :count *buffer-size*))
          (*socket-buffer-lisp* (make-array *buffer-size* :element-type '(unsigned-byte 8)))
          (callbacks nil))
      (incf *event-base-next-id*)
      (delay start-fn)
      ;; this is the once instance where we assign callbacks to a libevent object
      ;; instead of a data-pointer since the callbacks don't take any void* args,
      ;; meaning we have to dereference from the global (event-base-c *event-base*) object.
      (save-callbacks (event-base-c *event-base*) callbacks)
      (bt:with-lock-held (*event-base-registry-lock*)
        (setf (gethash (event-base-id *event-base*) *event-base-registry*) *event-base*))
      (unwind-protect
        (progn
          ;; this will block until all events are processed
          (uv:uv-run (event-base-c *event-base*) (cffi:foreign-enum-value 'uv:uv-run-mode :+uv-run-default+)))
        ;; cleanup
        (uv:uv-loop-close (event-base-c *event-base*))
        (process-event-loop-exit-callbacks)
        (cffi:foreign-free *socket-buffer-c*)
        (free-pointer-data (event-base-c *event-base*) :preserve-pointer t)
        (bt:with-lock-held (*event-base-registry-lock*)
          (remhash (event-base-id *event-base*) *event-base-registry*))
        (setf *event-base* nil)))))

(defmacro with-event-loop ((&key fatal-cb logger-cb default-event-cb (catch-app-errors nil catch-app-errors-supplied-p))
                           &body body)
  "Makes starting an event loop a tad less annoying. I really couldn't take
   typing out `(start-event-loop (lambda () ...) ...) every time. Example:

     (with-event-loop (:catch-app-errors t)
       (do-something-one-does-when-an-event-loop-is-running))

   See how nice that is?"
  (append
    `(as:start-event-loop (lambda () ,@body)
       :fatal-cb ,fatal-cb
       :logger-cb ,logger-cb
       :default-event-cb ,default-event-cb)
    (when catch-app-errors-supplied-p
      `(:catch-app-errors ,catch-app-errors))))
       

(defun exit-event-loop ()
  "Exit the event loop if running."
  (if (event-base-c *event-base*)
      (uv:uv-stop (event-base-c *event-base*))
      nil))


