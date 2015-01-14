(in-package :cl-async)

(defparameter *loop-close-iters* 100000
  "Maximum number of event loop cleanup iterations")

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

(defgeneric ref (handle)
  (:documentation
    "Reference a libuv handle object (uv_ref)"))

(defgeneric unref (handle)
  (:documentation
    "Unreference a libuv handle object (uv_unref)"))

(defun stats ()
  "Return statistics about the current event loop."
  (list :open-dns-queries (event-base-dns-ref-count *event-base*)
        :fn-registry-count (hash-table-count *function-registry*)
        :data-registry-count (hash-table-count *data-registry*)
        :incoming-tcp-connections (event-base-num-connections-in *event-base*)
        :outgoing-tcp-connections (event-base-num-connections-out *event-base*)))

(define-c-callback walk-cb :void ((handle :pointer) (arg :pointer))
  "Called when we're walking the loop."
  (declare (ignore arg))
  (format t "handle: ~s (~a)~%" (uv:handle-type handle) handle)
  (force-output))

(defun dump-event-loop-status ()
  "Return the status of the event loop. Really a debug function more than
   anything else."
  (check-event-loop-running)
  (uv:uv-walk (event-base-c *event-base*) (cffi:callback walk-cb) (cffi:null-pointer))
  (values))

(defvar *event-base-registry* (make-hash-table :test 'eq)
  "Holds ID -> event-base lookups for every active event loop. Mainly used when
   grabbing the threading context for a particular event loop.")

(defvar *event-base-registry-lock* (bt:make-lock)
  "Locks the event-base registry.")

(defgeneric handle-cleanup (handle-type handle)
  (:documentation "Perform cleanup for a libuv handle")
  (:method ((handle-type t) (handle t)) (values)))

(define-c-callback loop-exit-walk-cb :void ((handle :pointer) (arg :pointer))
  "Called when we want to close the loop AND IT WONT CLOSE. So we walk each
   handle and close them."
  (declare (ignore arg))
  (handle-cleanup (uv:handle-type handle) handle))

(defun do-close-loop (evloop)
  "Close an event loop by looping over its open handles, closing them, rinsing
   and repeating until uv-loop-close returns 0, but at most *LOOP-CLOSE-ITERS*
   times."
  (process-event-loop-exit-callbacks)
  (loop repeat *loop-close-iters*
        for res = (uv:uv-loop-close evloop)
        when (zerop res)
          return (values)
        else
          do (progn
               (uv:uv-stop evloop)
               (uv:uv-walk evloop (cffi:callback loop-exit-walk-cb) (cffi:null-pointer))
               (uv:uv-run evloop (cffi:foreign-enum-value 'uv:uv-run-mode :run-default))
               (uv:uv-run evloop (cffi:foreign-enum-value 'uv:uv-run-mode :run-default)))
        end
        finally
           (vom:error "failed to do loop cleanup in ~d iterations" *loop-close-iters*)
           (dump-event-loop-status)))

(defun start-event-loop (start-fn &key catch-app-errors (send-errors-to-eventcb t))
  "Simple wrapper function that starts an event loop which runs the given
   callback, most likely to init your server/client."
  (when *event-base*
    (error "Event loop already started. Please wait for it to exit."))
  (cffi:with-foreign-object (loop :unsigned-char (uv:uv-loop-size))
    (uv:uv-loop-init loop)
    ;; note the binding of these variable via (let), which means they are thread-
    ;; local... so this function can be called in different threads, and the bound
    ;; variables won't interfere with each other.
    (let* ((*event-base* (make-instance
                           'event-base
                           :c loop
                           :id *event-base-next-id*
                           :catch-app-errors catch-app-errors
                           :send-errors-to-eventcb send-errors-to-eventcb))
           (*buffer-writes* *buffer-writes*)
           (*buffer-size* *buffer-size*)
           (*output-buffer* (static-vectors:make-static-vector *buffer-size* :element-type 'octet))
           (*input-buffer* (static-vectors:make-static-vector *buffer-size* :element-type 'octet))
           (*data-registry* (event-base-data-registry *event-base*))
           (*function-registry* (event-base-function-registry *event-base*))
           (callbacks nil))
      (incf *event-base-next-id*)
      (delay start-fn)
      ;; this is the once instance where we assign callbacks to an event loop object
      ;; instead of a data-pointer since the callbacks don't take any void* args,
      ;; meaning we have to dereference from the global (event-base-c *event-base*) object.
      (save-callbacks (event-base-c *event-base*) callbacks)
      (bt:with-lock-held (*event-base-registry-lock*)
        (setf (gethash (event-base-id *event-base*) *event-base-registry*) *event-base*))
      (unwind-protect
        (progn
          ;; this will block until all events are processed
          (uv:uv-run (event-base-c *event-base*) (cffi:foreign-enum-value 'uv:uv-run-mode :run-default)))
        ;; cleanup
        (do-close-loop (event-base-c *event-base*))
        (static-vectors:free-static-vector *output-buffer*)
        (static-vectors:free-static-vector *input-buffer*)
        (free-pointer-data (event-base-c *event-base*) :preserve-pointer t)
        (bt:with-lock-held (*event-base-registry-lock*)
          (remhash (event-base-id *event-base*) *event-base-registry*))
        (setf *event-base* nil)))))

(defmacro with-event-loop ((&key catch-app-errors (send-errors-to-eventcb t))
                           &body body)
  "Makes starting an event loop a tad less annoying. I really couldn't take
   typing out `(start-event-loop (lambda () ...) ...) every time. Example:

     (with-event-loop (:catch-app-errors t)
       (do-something-one-does-when-an-event-loop-is-running))

   See how nice that is?"
   `(as:start-event-loop (lambda () ,@body)
       :catch-app-errors ,catch-app-errors
       :send-errors-to-eventcb ,send-errors-to-eventcb))

(defun exit-event-loop ()
  "Exit the event loop if running."
  (let ((evloop (event-base-c *event-base*)))
    (when evloop
      (uv:uv-stop evloop))))
