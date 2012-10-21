(in-package :cl-async)

(defclass future ()
  ((callbacks :accessor future-callbacks :initform nil)
   (finished :accessor future-finished :initform nil)
   (values :accessor future-values :initform nil))
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

(defun run-future (future &key clear)
  "Run all callbacks on a future *IF* the future is finished (and has computed
   values). If :clear is T, will remove callbacks after running them."
  (when (future-finished future)
    (let ((callbacks (future-callbacks future))
          (values (future-values future)))
      (dolist (cb callbacks)
        (apply cb values)))
    ;; clear out the callbacks if specified
    (when clear (setf (future-callbacks future) nil))
    future))

(defun finish (future &rest values)
  "Mark a future as finished, along with all values it's finished with."
  (setf (future-finished future) t
        (future-values future) values)
  (run-future future))

(defun attach-cb (future-return cb)
  "Attach a callbcak to a future. The future must be the first value in a list
   of values (car future-return) OR the future-return will be apply'ed to cb."
  (let* ((future (car future-return)))
    ;; if we were indeed passed a future, attach the callback to it AND run the
    ;; future if it has finished.
    (if (futurep future)
        (progn
          (push cb (future-callbacks future))
          (run-future future :clear t))
        (apply cb future-return))))

(defmacro attach (future-gen cb)
  "Macro wrapping attachment of callback to a future (takes multiple values into
   account, which a simple function cannot)."
  `(let* ((future-gen-vals (multiple-value-list ,future-gen)))
     (attach-cb future-gen-vals ,cb)))

