(in-package :cl-async)

(defvar *event-base* nil
  "THE event base (libevent) used to process all async operations.")
(defvar *fn-registry* nil
  "Function registry, allows the CFFI callbacks to run anonymous functions.")
(defvar *event-loop-end-functions* nil
  "Functions to call when the event loop closes")

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
  (let ((pointer (if (cffi:pointerp pointer)
                     (cffi:pointer-address pointer)
                     pointer))
        (callbacks (if (listp callbacks)
                       callbacks
                       (list callbacks))))
    (setf (gethash pointer *fn-registry*) callbacks)))

(defun get-callbacks (pointer)
  "Get all callbacks for the given object (pointer)."
  (when *fn-registry*
    (let ((pointer (if (cffi:pointerp pointer)
                       (cffi:pointer-address pointer)
                       pointer)))
      (gethash pointer *fn-registry*))))

(defun clear-callbacks (pointer)
  "Clear out all callbacks for the given object (pointer)."
  (when *fn-registry*
    (let ((pointer (if (cffi:pointerp pointer)
                       (cffi:pointer-address pointer)
                       pointer)))
      (remhash pointer *fn-registry*))))

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

(defun start-event-loop (start-fn)
  "Simple wrapper function that starts an event loop which runs the given
   callback, most likely to init your server/client."
  (if *event-base*
      (error "Event loop already started. Please wait for it to exit.")
      (let ((*event-base* (le:event-base-new)))
        (timer 0.0 start-fn)
        (unwind-protect
          (progn
            ;; this will block until all events are processed
            (le:event-base-dispatch *event-base*))
          (process-event-loop-exit-callbacks)
          (le:event-base-free *event-base*)
          (setf *event-base* nil)))))

(defun event-loop-exit ()
  "Exit the event loop if running."
  (if *event-base*
      (le:event-base-loopexit *event-base* (cffi:null-pointer))
      nil))

