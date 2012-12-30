(in-package :cl-async)

(define-condition event-info () ()
  (:documentation "Describes the base event for any action in cl-async.")
  (:report (lambda (c s) (format s "Info event: ~a" c))))

(define-condition event-error (event-info)
  ((code :initarg :code :reader event-errcode :initform 0)
   (msg :initarg :msg :reader event-errmsg :initform nil))
  (:report (lambda (c s) (format s "Error event: ~a: ~a" (event-errcode c) (event-errmsg c))))
  (:documentation "Describes a general error event."))

(defvar *catch-application-errors* nil
  "When t, permits cl-async to catch uncaught conditions in your application and
   pass them to the event-cb callback given. If no event-cb is given for the
   operation that triggered the condition, use *default-event-handler* as the
   event-cb.")

(defvar *signal-handlers* nil
  "Holds all the currently bound signal handlers, which can be used to unbind
   them all in one swift stroke.")

(defvar *default-event-handler*
  (lambda (err)
    ;; throw the error so we can wrap it in a handler-case
    (handler-case (error err)
      ;; got a connection error, throw it (must do this explicitely since
      ;; event-error extends event-info)
      (event-error () (error err))

      ;; this is just info, let it slide
      (event-info () nil)))
  "If an event-cb is not specified, this will be used as the event-cb IF
   *catch-application-errors* is set to t.")

(cffi:defcallback fatal-cb :void ((err :int))
  "Used to handle fatal libevent errors."
  (let ((fatal-cb (getf (get-callbacks *event-base*) :fatal-cb)))
    (when fatal-cb
      (funcall fatal-cb err))))

(cffi:defcallback logger-cb :void ((severity :int) (msg :string))
  "Used to catch log messages (if setup in start-event-loop)."
  (let ((logger-cb (getf (get-callbacks *event-base*) :logger-cb)))
    (when logger-cb
      (funcall logger-cb severity msg))))

(cffi:defcallback event-debug-cb :void ((severity :int) (msg :string))
  (let ((sev-str (cond
                   ((= severity le:+event-log-debug+) "debug")
                   ((= severity le:+event-log-msg+) "msg")
                   ((= severity le:+event-log-warn+) "warn")
                   ((= severity le:+event-log-err+) "err"))))
    (format t "EVDBG(~a): ~a~%" sev-str msg)
    (force-output)))

(defun enable-debug-mode ()
  "Enable debug mode. As far as I can tell, this is undoable, so you may have to
   restart your CL implementation after calling if you want to disable it.

   NOTE: This appears to make libevent exit suddenly after creating a second
   event loop for some reason. I recommend *not* using this function until I
   figure out debug omde a bit more."
  (if *event-base*
      (error "Debug mode must be enabled *before* an event loop has started.")
      (progn
        (le:event-enable-debug-mode)
        (le:event-set-log-callback (cffi:callback event-debug-cb)))))

(defun enable-threading-support ()
  "Enable threading support in libevent. This attempts to guess which threading
   function of libevent's to use and call it.

   Experimental, only supports pthreads or Windows."
  (let ((use-pthreads (cffi:foreign-symbol-pointer "evthread_use_pthreads"))
        (use-win-threads (cffi:foreign-symbol-pointer "evthread_use_windows_threads")))
    (cond
      (use-pthreads
        (cffi:foreign-funcall-pointer use-pthreads () :void))
      (use-win-threads
        (cffi:foreign-funcall-pointer use-win-threads () :void)))))

(defun stats ()
  "Return statistics about the current event loop."
  (list :open-dns-queries *dns-ref-count*
        :fn-registry-count (if (hash-table-p *fn-registry*)
                               (hash-table-count *fn-registry*)
                               0)
        :data-registry-count (if (hash-table-p *data-registry*)
                                 (hash-table-count *data-registry*)
                                 0)
        :incoming-tcp-connections *incoming-connection-count*
        :outgoing-tcp-connections *outgoing-connection-count*
        :incoming-http-connections *incoming-http-count*
        :outgoing-http-connections *outgoing-http-count*))

(defun dump-event-loop-status (file &key (return-as-string t))
  "Dump the status of the event loop to a file. Good for debugging.
   
   If return-as-string is T, the file is read/deleted and the contents returned
   as a string. Note that this is the default behavior."
  (check-event-loop-running)
  (let ((fp (cffi:foreign-funcall "fopen" :string (namestring file) :string "w+" :pointer)))
    (unwind-protect
        (le:event-base-dump-events *event-base* fp)
      (cffi:foreign-funcall "fclose" :pointer fp)))
  (when (and return-as-string
             (probe-file file))
    (unwind-protect
        (with-open-file (s file)
          (let* ((len (file-length s))
                 (data (make-string len)))
            (values data (read-sequence data s))))
      (delete-file file))))
  
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
  (let ((*catch-application-errors* (if catch-app-errors-supplied-p
                                        catch-app-errors
                                        *catch-application-errors*))
        (*default-event-handler* (if (functionp default-event-cb)
                                     default-event-cb
                                     *default-event-handler*))
        (*fn-registry* nil)
        (*data-registry* nil)
        (*event-loop-end-functions* nil)
        (*dns-base* nil)
        (*dns-ref-count* 0)
        (*signal-handlers* nil)
        (*socket-buffer-c* (cffi:foreign-alloc :unsigned-char :count *buffer-size*))
        (*socket-buffer-lisp* (make-array *buffer-size* :element-type '(unsigned-byte 8)))
        (*event-base* (le:event-base-new))
        (*incoming-connection-count* 0)
        (*outgoing-connection-count* 0)
        (*incoming-http-count* 0)
        (*outgoing-http-count* 0)
        (callbacks nil))
    (incf *event-base-id*)
    ;; set up a callback for dealing with fatal errors
    (when fatal-cb
      (setf callbacks (append callbacks (list :fatal-cb fatal-cb)))
      (le:event-set-fatal-callback (cffi:callback fatal-cb)))
    ;; set up the logging callback if we were passed one
    (when logger-cb
      (setf callbacks (append callbacks (list :logger-cb logger-cb)))
      (le:event-set-log-callback (cffi:callback logger-cb)))
    (delay start-fn)
    ;; this is the once instance where we assign callbacks to a libevent object
    ;; instead of a data-pointer since the callbacks don't take any void* args,
    ;; meaning we have to dereference from the global *event-base* object.
    (save-callbacks *event-base* callbacks)
    (unwind-protect
      (progn
        ;; this will block until all events are processed
        (le:event-base-dispatch *event-base*))
      ;; cleanup
      (process-event-loop-exit-callbacks)
      (cffi:foreign-free *socket-buffer-c*)
      (free-pointer-data *event-base* :preserve-pointer t)
      (le:event-base-free *event-base*)
      (setf *event-base* nil))))

(defun exit-event-loop ()
  "Exit the event loop if running."
  (if *event-base*
      (le:event-base-loopexit *event-base* (cffi:null-pointer))
      nil))

