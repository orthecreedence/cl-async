(in-package :cl-async)

(defvar *event-base* nil
  "THE event base (libevent) used to process all async operations.")
(defvar *fn-registry* nil
  "Function registry, allows the CFFI callbacks to run anonymous functions.")
(defvar *data-registry* nil
  "Data registry, gives CFFI callbacks access to anonymous data.")
(defvar *event-loop-end-functions* nil
  "Functions to call when the event loop closes")

(define-condition connection-info ()
  ((connection :initarg :connection :reader conn-fd :initform nil))
  (:report (lambda (c s) (format s "Connection info: ~a" (conn-fd c))))
  (:documentation "Describes the base condition for any action on a connection."))

(define-condition connection-error (connection-info)
  ((code :initarg :code :reader conn-errcode :initform 0)
   (msg :initarg :msg :reader conn-errmsg :initform nil))
  (:report (lambda (c s) (format s "Connection error: ~a: ~a" (conn-errcode c) (conn-errmsg c))))
  (:documentation "Describes a general connection error."))

(define-condition connection-eof (connection-info) ()
  (:report (lambda (c s) (format s "Connection EOF: ~a" (conn-fd c))))
  (:documentation "Passed to an event callback when a peer closes the connection."))

(define-condition connection-timeout (connection-error) ()
  (:report (lambda (c s) (format s "Connection timeout: ~a: ~a" (conn-errcode c) (conn-errmsg c))))
  (:documentation "Passed to an event callback when a connection times out."))

(define-condition connection-refused (connection-error) ()
  (:report (lambda (c s) (format s "Connection refused: ~a: ~a" (conn-errcode c) (conn-errmsg c))))
  (:documentation "Passed to an event callback when a connection is refused."))

(defvar *catch-application-errors* nil)
(defvar *default-event-handler*
  (lambda (err)
    ;; throw the error so we can wrap it in a handler-case
    (handler-case (error err)
      ;; this is just info, let it slide
      (connection-info () nil)
      ;; this an actual error. throw it back to toplevel (will exit the
      ;; event loop and cancel any pending events)
      (t () (error err))))
  "If an event-cb is not specified, this will always be used.")

(defmacro catch-app-errors (event-cb &body body)
  "Wraps catching of application errors into a simple handler-case (if wanted),
   otherwise just runs the body with no error/event handling."
  (let ((evcb (gensym)))
    `(if *catch-application-errors*
         (let ((,evcb (if (functionp ,event-cb)
                          ,event-cb
                          *default-event-handler*)))
           (handler-case
             (progn ,@body)
             (t (err) (funcall ,evcb err))))
         (progn ,@body))))
     
(defmacro make-foreign-type ((var type &key initial) bindings &body body)
  "Convenience macro, makes creation and initialization of CFFI types easier.
   Emphasis on initialization."
  `(cffi:with-foreign-object (,var ,type)
     (when ,initial
       (cffi:foreign-funcall "memset" :pointer ,var :unsigned-char ,initial :unsigned-char (cffi:foreign-type-size ,type)))
     ,@(loop for binding in bindings collect
         `(setf (cffi:foreign-slot-value ,var ,type ,(car binding)) ,(cadr binding)))
     ,@body))

(defun save-callbacks (pointer callbacks)
  "Save a set of callbacks, keyed by the given object (pointer)."
  (unless *fn-registry*
    (setf *fn-registry* (make-hash-table :test #'eql)))
  (let ((pointer (cffi:pointer-address pointer))
        (callbacks (if (listp callbacks)
                       callbacks
                       (list callbacks))))
    (setf (gethash pointer *fn-registry*) callbacks)))

(defun get-callbacks (pointer)
  "Get all callbacks for the given object (pointer)."
  (when *fn-registry*
    (let ((pointer (cffi:pointer-address pointer)))
      (gethash pointer *fn-registry*))))

(defun clear-callbacks (pointer)
  "Clear out all callbacks for the given object (pointer)."
  (when *fn-registry*
    (let ((pointer (cffi:pointer-address pointer)))
      (remhash pointer *fn-registry*))))

(defun create-data-pointer ()
  "Creates a pointer in C land that can be used to attach data/callbacks to.
   Note that this must be freed via clear-pointer-data."
  (cffi:foreign-alloc :char :count 0))

(defun attach-data-to-pointer (pointer data)
  "Attach a lisp object to a foreign pointer."
  (unless *data-registry*
    (setf *data-registry* (make-hash-table :test #'eql)))
  (let ((pointer (cffi:pointer-address pointer)))
    (setf (gethash pointer *data-registry*) data)))

(defun deref-data-from-pointer (pointer)
  "Grab data attached to a CFFI pointer."
  (when (and pointer *data-registry*)
    (let ((pointer (cffi:pointer-address pointer)))
      (gethash pointer *data-registry*))))

(defun clear-pointer-data (pointer)
  "Clear the data attached to a CFFI pointer."
  (when *data-registry*
    (let ((pointer (cffi:pointer-address pointer)))
      (remhash pointer *data-registry*))))

(defun free-pointer-data (pointer &key preserve-pointer)
  "Clears out all data attached to a foreign pointer, and frees the pointer."
  (when pointer
    (unwind-protect
      (progn
        (clear-callbacks pointer)
        (clear-pointer-data pointer))
      (unless preserve-pointer (cffi:foreign-free pointer)))))

(defun split-usec-time (time-s)
  "Given a second value, ie 3.67, return the number of seconds as the first
   value and the number of usecs for the second value."
  (if (numberp time-s)
      (multiple-value-bind (time-sec time-frac) (floor time-s)
        (values time-sec (floor (* 1000000 time-frac))))
      nil))

(defun append-array (arr1 arr2 &key element-type)
  "Create an array, made up of arr1 followed by arr2."
  (let ((arr1-length (length arr1))
        (arr2-length (length arr2)))
    (let ((arr (make-array (+ arr1-length arr2-length)
                           :element-type element-type)))
      (dotimes (i arr1-length)
        (setf (aref arr i) (aref arr1 i)))
      (dotimes (i arr2-length)
        (setf (aref arr (+ arr1-length i)) (aref arr2 i)))
      arr)))

(defun add-event-loop-exit-callback (fn)
  "Add a function to be run when the event loop exits."
  (push fn *event-loop-end-functions*))

(defun process-event-loop-exit-callbacks ()
  "run and clear out all event loop exit functions."
  (dolist (fn *event-loop-end-functions*)
    (funcall fn))
  (setf *event-loop-end-functions* nil))

(defun check-event-loop-running ()
  (unless *event-base*
    (error "Event loop not running. Start with function start-event-loop.")))

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

(defun enable-debug-mode ()
  "Enable debug mode. As far as I can tell, this is undoable, so you may have to
   restart your CL implementation after calling if you want to disable it.

   NOTE: This appears to make libevent exit suddenly after creating a second
   event loop for some reason. I recommend *not* using this function until I
   figure out debug omde a bit more."
  (if *event-base*
      (error "Debug mode must be enabled *before* an event loop has started.")
      (le:event-enable-debug-mode)))

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
        (*event-base* (le:event-base-new))
        (callbacks nil))
    ;; set up a callback for dealing with fatal errors
    (when fatal-cb
      (setf callbacks (append callbacks (list :fatal-cb fatal-cb)))
      (le:event-set-fatal-callback (cffi:callback fatal-cb)))
    ;; set up the logging callback if we were passed one
    (when logger-cb
      (setf callbacks (append callbacks (list :logger-cb logger-cb)))
      (le:event-set-log-callback (cffi:callback logger-cb)))
    (delay start-fn)
    (save-callbacks *event-base* callbacks)
    (unwind-protect
      (progn
        ;; this will block until all events are processed
        (le:event-base-dispatch *event-base*))
      (process-event-loop-exit-callbacks)
      (le:event-base-free *event-base*)
      (setf *event-base* nil))))

(defun event-loop-exit ()
  "Exit the event loop if running."
  (if *event-base*
      (le:event-base-loopexit *event-base* (cffi:null-pointer))
      nil))

