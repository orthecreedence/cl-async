(in-package :cl-async)

(defclass future ()
  ((callbacks :accessor future-callbacks :initform nil
    :documentation "A list that holds all callbacks associated with this future.")
   (event-handler :accessor future-event-handler :initarg :event-handler :initform nil
    :documentation "Holds a callback that will handle events as they happen on
                    the future. If events occur and there is no handler, they
                    will be saved, in order, and sent to the handler once one is
                    attached.")
   (preserve-callbacks :accessor future-preserve-callbacks :initarg :preserve-callbacks :initform nil
    :documentation "When nil (the default) detaches callbacks after running
                    future.")
   (finished :accessor future-finished :initform nil
    :documentation "Marks if a future has been finished or not.")
   (events :accessor future-events :initform nil
    :documentation "Holds events for this future, to be handled with event-handler.")
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

(defun run-event-handler (future)
  "If an event handler exists for this future, run all events through the
   handler and clear the events out once run."
  (let ((event-handler (future-event-handler future)))
    (when event-handler
      (dolist (event (nreverse (future-events future)))
        (funcall event-handler event))
      (setf (future-events future) nil))))

(defun set-event-handler (future cb)
  "Sets the event handler for a future. If the handler is attached after events
   have already been caught, they will be passed into the handler, in order,
   directly after it is added."
  (setf (future-event-handler future) cb)
  (run-event-handler future))

(defun signal-event (future condition)
  "Signal that an event has happened on a future. If the future has an event
   handler, the given condition will be passed to it, otherwise the event will
   be saved until an event handler has been attached."
  (push condition (future-events future))
  (run-event-handler future))

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

