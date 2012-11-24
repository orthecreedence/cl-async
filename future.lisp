(in-package :cl-async-future)

(defclass future ()
  ((callbacks :accessor future-callbacks :initform nil
    :documentation "A list that holds all callbacks associated with this future.")
   (errbacks :accessor future-errbacks :initform nil
    :documentation "A list that holds all errbacks associated with this future.")
   (forwarded-future :accessor future-forward-to :initform nil
    :documentation "Can hold a reference to another future, which will receive
                    callbacks and event handlers added to this one once set.
                    This allows a future to effectively take over another future
                    by taking all its callbacks/events.")
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
  (format s "#<Future (~s callbacks) (errbacks: ~s) (finished: ~a) (forward: ~a)>"
          (length (future-callbacks future))
          (length (future-errbacks future))
          (future-finished future)
          (not (not (future-forward-to future)))))

(defun make-future (&key preserve-callbacks (reattach-callbacks t))
  "Create a blank future."
  (make-instance 'future :preserve-callbacks preserve-callbacks
                         :reattach-callbacks reattach-callbacks))

(defun futurep (future)
  "Is this a future?"
  (subtypep (type-of future) 'future))

(defun do-add-callback (future cb)
  "Add a callback to a future if it isn't already attached."
  (unless (member cb (future-callbacks future))
    (push cb (future-callbacks future))))

(defun attach-errback (future errback)
  "Add an error handler for this future. If the errback already exists on this
   future, don't re-add it."
  (when (futurep future)
    (let ((forwarded-future (lookup-actual-future future)))
      (unless (member errback (future-errbacks future))
        (push errback (future-errbacks forwarded-future))
        (process-errors forwarded-future))))
  future)

(defun setup-future-forward (future-from future-to)
  "Set up future-from to send all callbacks, events, handlers, etc to the
   future-to future. This includes all current objects, plus objects that may be
   added later on. For instance, if you forward future A to future B, adding an
   event handler to future A will then add it to future B (assuming future B has
   no current event handler). The same goes for callbacks as well, they will be
   added to the new future-to if added to the future-from."
  ;; a future "returned" another future. reattach the callbacks/errbacks from
  ;; the original future onto the returned one
  (dolist (cb (future-callbacks future-from))
    (do-add-callback future-to cb))
  (dolist (errback (future-errbacks future-from))
    (attach-errback future-to errback))
  ;; mark the future as forwarded to other parts of the system know to use the
  ;; new future for various tasks.
  (setf (future-forward-to future-from) future-to))

(defun lookup-actual-future (future)
  "This function follows forwarded futures until it finds the last in the chain
   of forwarding."
  (when (futurep future)
    (loop while (future-forward-to future) do
      (setf future (future-forward-to future))))
  future)

(defun process-errors (future)
  "If an event handler exists for this future, run all events through the
   errbacks and clear the events out once run."
  (when (future-errbacks future)
    (dolist (ev (reverse (future-events future)))
      (dolist (errback (reverse (future-errbacks future)))
        (funcall errback ev)))
    (setf (future-events future) nil)))

(defun signal-error (future condition)
  "Signal that an error has happened on a future. If the future has errbacks,
   they will be used to process the error, otherwise it will be stored until an
   errback is added to the future."
  (let ((forwarded-future (lookup-actual-future future)))
    (push condition (future-events forwarded-future))
    (process-errors forwarded-future)))

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
  "Mark a future as finished, along with all values it's finished with. If
   finished with another future, forward the current future to the new one."
  (let ((new-future (car values)))
    (cond ((and (futurep new-future)
                (future-reattach-callbacks future))
           ;; set up the current future to forward all callbacks/handlers/events
           ;; to the new future from now on.
           (setup-future-forward future new-future)
           ;; run the new future
           (run-future new-future))
          (t
           ;; just a normal finish, run the future
           (setf (future-finished future) t
                 (future-values future) values)
           (run-future future)))))

(defun attach-cb (future-values cb)
  "Attach a callback to a future. The future must be the first value in a list
   of values (car future-values) OR the future-values will be apply'ed to cb."
  (let* ((future future-values)
         (future (if (futurep future)
                     (lookup-actual-future future)  ; follow forwarded futures
                     (car future-values)))
         (cb-return-future (make-future))
         (cb-wrapped (lambda (&rest args)
                       (let ((cb-return (multiple-value-list (apply cb args))))
                         (apply #'finish (append (list cb-return-future)
                                                 cb-return))))))
    ;; if we were indeed passed a future, attach the callback to it AND run the
    ;; future if it has finished.
    (if (futurep future)
        (progn
          (do-add-callback future cb-wrapped)
          (run-future future))
        ;; not a future, just a value. run the callback directly
        (apply cb-wrapped future-values))
    cb-return-future))

(defmacro attach (future-gen cb)
  "Macro wrapping attachment of callback to a future (takes multiple values into
   account, which a simple function cannot)."
  `(let ((future-gen-vals (multiple-value-list ,future-gen)))
     (cl-async-future::attach-cb future-gen-vals ,cb)))

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
         (num-bindings (length bindings))
         (finished-future (gensym "finished-future"))
         (finished-vals (gensym "finished-vals"))
         (finished-cb (gensym "finished-cb"))
         (args (gensym "args")))
    `(let* ((,finished-future (make-future))
            (,finished-vals nil)
            (,finished-cb
              (let ((c 0))
                (lambda ()
                  (incf c)
                  (when (<= ,num-bindings c)
                    (let ((vars (loop for bind in ',bind-vars collect (getf ,finished-vals bind))))
                      (apply #'finish (append (list ,finished-future) vars))))))))
       ;; for each binding, attach a callback to the future it generates that
       ;; marks itself as complete. once all binding forms report in, the main
       ;; future "finished-future" is triggered, which runs the body
       ,@(loop for (bind form) in bindings collect
           `(let ((future-gen (multiple-value-list ,form)))
              ;; forward events we get on this future to the finalizing future.
              (attach-errback (car future-gen)
                (lambda (ev) (signal-error ,finished-future ev)))
              ;; when this future finishes, call the finished-cb, which tallies
              ;; up the number of finishes until it equals the number of
              ;; bindings.
              (attach (apply #'values future-gen)
                (lambda (&rest ,args)
                  (setf (getf ,finished-vals ',bind) (car ,args))
                  (funcall ,finished-cb)))))
       ;; return our future which gets fired when all bindings have completed.
       ;; gets events forwarded to it from the binding futures.
       (attach ,finished-future
         (lambda ,bind-vars
           ,(when ignore-bindings
              `(declare (ignore ,@ignore-bindings)))
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
             (bind (if ignore-bind (gensym "async-ignore") bind))
             (future (cadr binding))
             (args (gensym "args")))
        ;; since this is in the tail-position, no need to explicitely set
        ;; callbacks/event handler since they will be reattached automatically.
        `(attach ,future
           (lambda (&rest ,args)
             (let ((,bind (car ,args)))
               ,(when ignore-bind `(declare (ignore ,bind)))
               ;; being recursive helps us keep the code cleaner...
               (alet* ,(cdr bindings) ,@body)))))
      `(progn ,@body)))

(defmacro multiple-future-bind ((&rest bindings) future-gen &body body)
  "Like multiple-value-bind, but instead of a form that evaluates to multiple
   values, takes a form that generates a future."
  (let ((args (gensym "args")))
    `(attach ,future-gen
       (lambda (&rest ,args)
         (let (,@bindings)
           ,@(loop for b in bindings collect
               `(setf ,b (car ,args)
                      ,args (cdr ,args)))
           ,@body)))))

(defmacro wait-for (future-gen &body body)
  "Wait for a future to finish, ignoring any values it returns. Can be useful
   when you want to run an async action but don't care about the return value
   (or it doesn't return a value) and you want to continue processing when it
   returns."
  (let ((ignore-var (gensym "async-ignore")))
    `(attach ,future-gen
       (lambda (&rest ,ignore-var)
         (declare (ignore ,ignore-var))
         ,@body))))

(defmacro wrap-event-handler (future-gen error-forms)
  "Used to wrap the future-generation forms of future syntax macros. This macro
   is not to be used directly, but instead by future-handler-case.
   
   It allows itself to be recursive, but any recursions will simply add their
   error forms for a top-level list and return the form they are given as the
   body. This allows a top-level form to add an error handler to a future, while
   gathering the lower-level forms' handler-case bindings into one big handler
   function (created with make-nexted-handler-cases).
   
   Note that since normally the wrap-event-handler forms expand outside in, we
   have to do some trickery with the error-handling functions to make sure the
   order of the handler-case forms (as far as what level of the tree we're on)
   are preserved."
  ;; define a macrolet that takes all sub `wrap-event-handler` forms and
  ;; rewrites them to inject their error handlers into a lambda chain that makes
  ;; sure the handler-case tree is in the correct order (just wrapping one
  ;; lambda in the next will reverse the order).
  `(macrolet ((wrap-event-handler (future-gen error-forms)
               `(progn
                  ;; "inject" the next-level down error handler in between the
                  ;; error triggering function and the error handler one level
                  ;; up. this preserves the handler-case tree (as opposed to 
                  ;; reversing it)
                  (let ((old-signal-error signal-error))
                    (setf signal-error 
                          (lambda (ev)
                            (handler-case
                              (funcall old-signal-error ev)
                              ,@error-forms))))
                  ,future-gen)))
     ;; define a function that signals the error, and a top-level error handler
     ;; which uses the error-forms passed to THIS macro instance. any instance
     ;; of `wrap-event-handler` that occurs in the `future-gen` form will inject
     ;; its error handler between handler-fn and signal-error.
     (let* ((signal-error (lambda (ev) (error ev)))
            (handler-fn (lambda (ev)
                          (handler-case
                            (funcall signal-error ev)
                            ,@error-forms)))
            (vals (multiple-value-list ,future-gen)))
       (if (futurep (car vals))
           (progn
             (attach-errback (car vals) handler-fn))
           (apply #'values vals)))))

(defmacro future-handler-case (body-form &rest error-forms &environment env)
  "Wrap all of our lovely syntax macros up with an event handler. This is more
   or less restricted to the form it's run in."
  ;; save the original macro functions so they aren't overwritten by macrolet,
  ;; the slithering scope-stealing snake. if future-handler-case is called
  ;; inside another future-handler-case, these "*-orig" functions will be bound
  ;; to the wrapping macrolet form instead of the top-level macros, which is
  ;; perfect because we want to wrap the forms multiple times.
  (let ((attach-orig (macro-function 'attach env))
        (alet-orig (macro-function 'alet env))
        (alet*-orig (macro-function 'alet* env))
        (multiple-future-bind-orig (macro-function 'multiple-future-bind env))
        (wait-for-orig (macro-function 'wait-for env)))
    ;; wrap the top-level form in a handler-case to catch any errors we may have
    ;; before the futures are even generated.
    `(handler-case
       ;; redefine all our syntax macros so that the future-gen forms are
       ;; wrapped (recursively, if called more than once) in the
       ;; `wrap-event-handler` macro, which will setup a single error handler
       ;; for each form that takes all the handler-case forms passed into
       ;; future-handler-case into account.
       (macrolet ((attach (future-gen fn)
                    (funcall ,attach-orig
                      `(attach
                         (wrap-event-handler ,future-gen ,',error-forms)
                         ,fn)
                      ,env))
                  (alet (bindings &body body)
                    (funcall ,alet-orig
                      `(alet ,(loop for (bind form) in bindings
                                    collect `(,bind (wrap-event-handler ,form ,',error-forms)))
                         ,@body)
                      ,env))
                  (alet* (bindings &body body)
                    (funcall ,alet*-orig
                      `(alet* ,(loop for (bind form) in bindings
                                     collect `(,bind (wrap-event-handler ,form ,',error-forms)))
                         ,@body)
                      ,env))
                  (multiple-future-bind (bindings future-gen &body body)
                    (funcall ,multiple-future-bind-orig
                      `(multiple-future-bind ,bindings
                           (wrap-event-handler ,future-gen ,',error-forms)
                         ,@body)
                      ,env))
                  (wait-for (future-gen &body body)
                    (funcall ,wait-for-orig
                      `(wait-for (wrap-event-handler ,future-gen ,',error-forms)
                         ,@body)
                      ,env)))
           ,body-form)
       ,@error-forms)))

