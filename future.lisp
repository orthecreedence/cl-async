(in-package :cl-async-future)

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
   (reattach-callbacks :accessor future-reattach-callbacks :initarg :reattach-callbacks :initform t
    :documentation "When a future's callback returns another future, bind all
                    callbacks from this future onto the returned one. Allows
                    values to transparently be derived from many layers deep of
                    futures, almost like a real call stack.")
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

(defmethod print-object ((future future) s)
  (format s "#<Future (~s callbacks) (ev handler: ~s) (finished: ~a)>" (length (future-callbacks future)) (not (not (future-event-handler future))) (future-finished future)))

(defun make-future (&key preserve-callbacks (reattach-callbacks t))
  "Create a blank future."
  (make-instance 'future :preserve-callbacks preserve-callbacks
                         :reattach-callbacks reattach-callbacks))

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
  (let ((first-val (car values)))
    (cond ((and (futurep first-val)
                (future-reattach-callbacks future))
           ;; a future "returned" another future. reattach the callbacks from
           ;; the original future onto the returned on
           (setf (future-callbacks first-val) (future-callbacks future))
           ;; if the new future doesnt have an explicit error handler, attach
           ;; the handler from one future level up
           (unless (future-event-handler first-val)
             (setf (future-event-handler first-val) (future-event-handler future)))
           (run-future first-val))
          (t
           (setf (future-finished future) t
                 (future-values future) values)
           (run-future future)))))

(defun attach-cb (future-values cb)
  "Attach a callback to a future. The future must be the first value in a list
   of values (car future-values) OR the future-values will be apply'ed to cb."
  (let* ((future future-values)
         (future (if (futurep future) future (car future-values)))
         (cb-return-future (make-future))
         (cb-wrapped (lambda (&rest args)
                       (let ((cb-return (multiple-value-list (apply cb args))))
                         (apply #'finish (append (list cb-return-future)
                                                 cb-return))))))
    ;; if we were indeed passed a future, attach the callback to it AND run the
    ;; future if it has finished.
    (if (futurep future)
        (progn
          (push cb-wrapped (future-callbacks future))
          (run-future future))
        ;; not a future, just a value. run the callback directly
        (apply cb-wrapped future-values))
    cb-return-future))

(defmacro attach (future-gen cb)
  "Macro wrapping attachment of callback to a future (takes multiple values into
   account, which a simple function cannot)."
  `(let ((future-gen-vals (multiple-value-list ,future-gen)))
     (attach-cb future-gen-vals ,cb)))

;; -----------------------------------------------------------------------------
;; start our syntactic abstraction section (rolls off the tongue nicely)
;; -----------------------------------------------------------------------------

(defmacro alet (bindings &body body)
  "Asynchronous let. Allows calculating a number of values in parallel via
   futures, and runs the body when all values have computed with the bindings
   given available to the body.
   
   Also returns a future that fires with the values returned from the body form,
   which allows arbitrary nesting to get a final value(s)."
  (let* ((ignore-bindings nil)
         (bindings (loop for (bind form) in bindings
                         collect (list (if bind
                                           bind
                                           (let ((igsym (gensym "alet-ignore")))
                                             (push igsym ignore-bindings)
                                             igsym))
                                       form)))
         (bind-vars (loop for (bind nil) in bindings collect bind))
         (num-bindings (length bindings)))
    `(let* ((finished-future (make-future))
            (finished-vals nil)
            (finished-cb
              (let ((c 0))
                (lambda ()
                  (incf c)
                  (when (<= ,num-bindings c)
                    (let ((vars (loop for bind in ',bind-vars collect (getf finished-vals bind))))
                      (apply #'finish (append (list finished-future) vars))))))))
       ,@(loop for (bind form) in bindings collect
           `(attach ,form
                    (lambda (&rest vals)
                      (setf (getf finished-vals ',bind) (car vals))
                      (funcall finished-cb))))
       (attach finished-future
         (lambda ,bind-vars
           (declare (ignore ,@ignore-bindings))
           ,@body)))))

(defmacro alet* (bindings &body body)
  "Asynchronous let*. Allows calculating a number of values in sequence via
   futures, and run the body when all values have computed with the bindings
   given available to the body.
   
   Also returns a future that fires with the values returned from the body form,
   which allows arbitrary nesting to get a final value(s)."
  (if bindings
      (let* ((binding (car bindings))
             (bind (car binding))
             (ignore-bind (not bind))
             (bind (if ignore-bind '_ bind))
             (future (cadr binding)))
        `(attach ,future
           (lambda (&rest args)
             (let ((,bind (car args)))
               ,(when ignore-bind
                  `(declare (ignore ,bind)))
               (alet* ,(cdr bindings) ,@body)))))
      `(progn ,@body)))

(defmacro multiple-future-bind ((&rest bindings) future-gen &body body)
  "Like multiple value bind, but instead of a form that evaluates to multiple
   values, takes a form that generates a future."
  `(attach ,future-gen
     (lambda (,@bindings) ,@body)))

(defmacro wait-for (future-gen &body body)
  "Wait for a future to finish, ignoring any values it returns. Can be useful
   when you sent a command to a server and you don't really care what the
   response is, you just want to run the body when it returns."
  `(attach ,future-gen
     (lambda (&rest _)
       (declare (ignore _))
       ,@body)))

