(in-package :cl-async)

(defclass future ()
  ((callbacks :accessor future-callbacks :initform nil
    :documentation "A list that holds all callbacks associated with this future.")
   (preserve-callbacks :accessor future-preserve-callbacks :initarg :preserve-callbacks :iniftform nil
    :documentation "When nil (the default) detaches callbacks after running
                    future.")
   (finished :accessor future-finished :initform nil
    :documentation "Marks if a future has been finished or not.")
   (values :accessor future-values :initform nil
    :documentation "Holds the finished value(s) of the computer future. Will be
                    apply'ed to the callbacks."))
  (:documentation
    "Defines a class which represents a value that MAY be ready sometime in the
     future. Also supports attaching callbacks to the future such that they will
     be called with the computed value(s) when ready."))

(defun make-future ()
  "Create a blank future."
  (make-instance 'future))

(defun futurep (future)
  "Is this a future?"
  (subtypep (type-of future) 'future))

(defun run-future (future)
  "Run all callbacks on a future *IF* the future is finished (and has computed
   values). If preserve-callbacks in the future is set to nil, the future's
   callbacks will be detached after running."
  (when (future-finished future)
    (let ((callbacks (future-callbacks future))
          (values (future-values future)))
      (dolist (cb (reverse callbacks))
        (apply cb values)))
    ;; clear out the callbacks if specified
    (unless (future-preserve-callbacks future)
      (setf (future-callbacks future) nil))
    future))

(defun finish (future &rest values)
  "Mark a future as finished, along with all values it's finished with."
  (setf (future-finished future) t
        (future-values future) values)
  (run-future future))

(defun attach-cb (future-return cb)
  "Attach a callback to a future. The future must be the first value in a list
   of values (car future-return) OR the future-return will be apply'ed to cb."
  (let* ((future future-return)
         (future (if (futurep future) future (car future-return))))
    ;; if we were indeed passed a future, attach the callback to it AND run the
    ;; future if it has finished.
    (if (futurep future)
        (progn
          (push cb (future-callbacks future))
          (run-future future))
        ;; not a future, just a value. run the callback directly
        (apply cb future-return))))

(defmacro attach (future-gen cb)
  "Macro wrapping attachment of callback to a future (takes multiple values into
   account, which a simple function cannot)."
  `(let* ((future-gen-vals (multiple-value-list ,future-gen)))
     (attach-cb future-gen-vals ,cb)))

