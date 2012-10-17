(in-package :cl-async)

(defvar *event-base* nil
  "THE event base (libevent) used to process all async operations.")
(defvar *fn-registry* nil
  "Function registry, allows the CFFI callbacks to run anonymous functions.")
(defvar *data-registry* nil
  "Data registry, gives CFFI callbacks access to anonymous data.")
(defvar *event-loop-end-functions* nil
  "Functions to call when the event loop closes")

(defvar *dns-base* nil
  "Holds the evdns-base object used for DNS lookups. One per event loop should
   suffice.")

(defvar *dns-ref-count* 0
  "Counts how many open DNS queries there are, and allows freeing the DNS base
   once there are no more references.")

;; consider somehow moving these to tcp.lisp without creating circular deps
;; (since they are initialized in start-event-loop as thread-local vars)
(defparameter *buffer-size* 16384
  "The amount of data we'll pull from the evbuffers when doing reading/writing.")
(defvar *socket-buffer-c* nil
  "A pointer to the buffer in C land that reads from sockets.")
(defvar *socket-buffer-lisp* nil
  "An array in lisp land that holds data copied from a socket.")

(defvar *incoming-connection-count* 0
  "Number of incoming TCP connections.")
(defvar *outgoing-connection-count* 0
  "Number of outgoing TCP connections.")
(defvar *incoming-http-count* 0
  "Number of incoming HTTP connections.")
(defvar *outgoing-http-count* 0
  "Number of outgoing HTTP connections.")

(defvar *catch-application-errors* nil
  "When t, permits cl-async to catch uncaught conditions in your application and
   pass them to the event-cb callback given. If no event-cb is given for the
   operation that triggered the condition, use *default-event-handler* as the
   event-cb.")

(defvar *signal-handlers* nil
  "Holds all the currently bound signal handlers, which can be used to unbind
   them all in one swift stroke.")

;; define some cached values to save CFFI calls. believe it or not, this does
;; make a performance difference
(defconstant +sockaddr-size+ (cffi:foreign-type-size (le::cffi-type le::sockaddr-in)))
(defconstant +evutil-addrinfo-size+ (cffi:foreign-type-size (le::cffi-type le::evutil-addrinfo)))
(defconstant +timeval-size+ (cffi:foreign-type-size (le::cffi-type le::timeval)))
(defconstant +bev-opt-close-on-free+ (cffi:foreign-enum-value 'le:bufferevent-options :+bev-opt-close-on-free+))

(define-condition connection-info () ()
  (:documentation "Describes the base condition for any action on a connection."))

(define-condition connection-error (connection-info)
  ((code :initarg :code :reader conn-errcode :initform 0)
   (msg :initarg :msg :reader conn-errmsg :initform nil))
  (:report (lambda (c s) (format s "Connection error: ~a: ~a" (conn-errcode c) (conn-errmsg c))))
  (:documentation "Describes a general connection error."))

(defvar *default-event-handler*
  (lambda (err)
    ;; throw the error so we can wrap it in a handler-case
    (handler-case (error err)
      ;; got a connection error, throw it (must do this explicitely since
      ;; connection-error extends connection-info)
      (connection-error () (error err))

      ;; this is just info, let it slide
      (connection-info () nil)
      
      ;; this an actual error. throw it back to toplevel
      (t () (error err))))
  "If an event-cb is not specified, this will be used as the event-cb IF
   *catch-application-errors* is set to t.")

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
     
(defmacro make-foreign-type ((var type &key initial type-size) bindings &body body)
  "Convenience macro, makes creation and initialization of CFFI types easier.
   Emphasis on initialization."
  `(cffi:with-foreign-object (,var ,type)
     ,(when initial
        `(cffi:foreign-funcall "memset" :pointer ,var :unsigned-char ,initial :unsigned-char ,(if type-size type-size `(cffi:foreign-type-size ,type))))
     ,@(loop for binding in bindings collect
         `(setf (cffi:foreign-slot-value ,var ,type ,(car binding)) ,(cadr binding)))
     ,@body))

(defun make-pointer-eql-able (pointer)
  "Abstraction to make a CFFI pointer #'eql to itself. Does its best to be the
   most performant for the current implementation."
  (when pointer
    #+(or ccl)
      pointer
    #-(or ccl)
      (if (cffi:pointerp pointer)
          (cffi:pointer-address pointer)
          pointer)))

(defun create-data-pointer ()
  "Creates a pointer in C land that can be used to attach data/callbacks to.
   Note that this must be freed via clear-pointer-data."
  (cffi:foreign-alloc :char :count 1))

(defun save-callbacks (pointer callbacks)
  "Save a set of callbacks, keyed by the given pointer."
  (unless *fn-registry*
    (setf *fn-registry* (make-hash-table :test #'eql)))
  (let ((callbacks (if (listp callbacks)
                       callbacks
                       (list callbacks))))
    (setf (gethash (make-pointer-eql-able pointer) *fn-registry*) callbacks)))

(defun get-callbacks (pointer)
  "Get all callbacks for the given pointer."
  (when *fn-registry*
    (gethash (make-pointer-eql-able pointer) *fn-registry*)))

(defun clear-callbacks (pointer)
  "Clear out all callbacks for the given pointer."
  (when *fn-registry*
    (remhash (make-pointer-eql-able pointer) *fn-registry*)))

(defun attach-data-to-pointer (pointer data)
  "Attach a lisp object to a foreign pointer."
  (unless *data-registry*
    (setf *data-registry* (make-hash-table :test #'eql)))
  (setf (gethash (make-pointer-eql-able pointer) *data-registry*) data))

(defun deref-data-from-pointer (pointer)
  "Grab data attached to a CFFI pointer."
  (when (and pointer *data-registry*)
    (gethash (make-pointer-eql-able pointer) *data-registry*)))

(defun clear-pointer-data (pointer)
  "Clear the data attached to a CFFI pointer."
  (when (and pointer *data-registry*)
    (remhash (make-pointer-eql-able pointer) *data-registry*)))

(defun free-pointer-data (pointer &key preserve-pointer)
  "Clears out all data attached to a foreign pointer, and frees the pointer
   (unless :preserve-pointer is t)."
  (when pointer
    (unwind-protect
      (progn
        (clear-callbacks pointer)
        (clear-pointer-data pointer))
      (unless preserve-pointer
        (when (cffi:pointerp pointer)
          (cffi:foreign-free pointer))))))

(defun split-usec-time (time-s)
  "Given a second value, ie 3.67, return the number of seconds as the first
   value and the number of usecs for the second value."
  (if (numberp time-s)
      (multiple-value-bind (time-sec time-frac) (floor time-s)
        (values time-sec (floor (* 1000000 time-frac))))
      nil))

(defun append-array (arr1 arr2)
  "Create an array, made up of arr1 followed by arr2."
  (let ((arr1-length (length arr1))
        (arr2-length (length arr2)))
    (let ((arr (make-array (+ arr1-length arr2-length)
                           :element-type (array-element-type arr1))))
      (replace arr arr1 :start1 0)
      (replace arr arr2 :start1 arr1-length)
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

