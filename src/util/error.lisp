(in-package :cl-async-util)

(defvar *passthrough-errors* '())

(defun passthrough-error-p (e)
  "Return true if the specified error condition should not
  be intercepted by cl-async error handling mechanism."
  (loop for type in *passthrough-errors*
          thereis (typep e type)))

(defgeneric handle-error (error)
  (:documentation
    "Default error handler responsible for handling uncaught errors."))

(defmethod handle-error ((error t))
  (vom:warn "handle-error: ~a" error))

(defun call-with-callback-restarts (thunk &key continue-fn (abort-restart-description "Abort cl-async callback."))
  "Call thunk with restarts that make it possible to ignore the callback
  in case of an error or safely terminate the event loop.

  If SWANK is active, set SLDB's quit restart to ABORT-CALLBACK restart
  if *safe-sldb-quit-restart* is true or EXIT-EVENT-LOOP otherwise."
  (restart-case
      ;; have to use the ugly symbol-value hack instead of #+swank / #-swank
      ;; in order to avoid compiling in SWANK (in)dependency
      (let* ((swank-package (find-package :swank))
             (quit-restart-sym (when swank-package
                                 (find-symbol (symbol-name '#:*sldb-quit-restart*)
                                              swank-package))))
        (if quit-restart-sym
            (let ((old-quit-restart (symbol-value quit-restart-sym)))
              (setf (symbol-value quit-restart-sym)
                    (if *safe-sldb-quit-restart*
                        'abort-callback
                        'exit-event-loop))
              (unwind-protect
                   (funcall thunk)
                (setf (symbol-value quit-restart-sym) old-quit-restart)))
            (funcall thunk)))
    (continue-event-loop ()
      :report "Continue the event loop, passing the error to the default handler."
      (format *debug-io* "~&;; event loop continued (main handler)~%")
      (funcall continue-fn)
      (values))
    (abort-callback ()
      :report (lambda (stream) (write-string abort-restart-description stream))
      (format *debug-io* "~&;; callback aborted~%")
      (values))
    (exit-event-loop ()
      :report "Exit the current event loop."
      (format *debug-io* "~&;; exiting the event loop.~%")
      (uv:uv-stop (event-base-c *event-base*)))))

(defvar *evcb-err*)

(defmacro catch-app-errors (event-cb &body body)
  "Handle error conditions by directing them to the specified event
   callback or default event handler of the current event loop, if
   catching app errors is enabled for the current event loop via
   EVENT-BASE-CATCH-APP-ERRORS, otherwise just evaluate the BODY.

   If event-cbs are called via run-event-cb, make sure the event-cb
   is NOT double-called with the same condition twice."
  (alexandria:once-only (event-cb)
    (alexandria:with-gensyms (err last-err thunk-fn continue-fn blk)
      `(let ((*evcb-err* '())
             (,last-err nil))
         (labels ((,continue-fn (error)
                    (let* ((handler (when (event-base-send-errors-to-eventcb *event-base*)
                                      ,event-cb))
                           (handler (or handler
                                        (event-base-catch-app-errors *event-base*)))
                           (handler (if (and handler
                                             (not (typep handler 'boolean)))
                                         handler
                                         'handle-error)))
                      (when (and handler error)
                        (funcall handler error))))
                  (,thunk-fn ()
                    (call-with-callback-restarts
                      (lambda () ,@body)
                      :continue-fn (lambda () (,continue-fn ,last-err)))))
           (block ,blk
             (handler-bind
                 ((error (lambda (,err)
                           (setf ,last-err ,err)
                           ;; check whether the error was already sent to eventcb
                           (unless (or (member ,err *evcb-err*)
                                       (passthrough-error-p ,err))
                             (when (event-base-catch-app-errors *event-base*)
                               (,continue-fn ,err)
                               (return-from ,blk))))))
               (,thunk-fn))))))))

(defun run-event-cb (event-cb &rest args)
  "When used in the dynamic context of catch-app-errors, wraps the
   calling of an event-cb with args such that errors are caught and
   saved, ensuring that an event-cb isn't called twice with the same
   condition. When used outside the dynamic context of
   catch-app-errors, just invokes event-cb with args."
  (if (boundp '*evcb-err*)
      (handler-bind
          ;; catch any errors and track them
          ((error #'(lambda (err)
                      ;; track the error so we don't re-fire (*evcb-err* is bound in
                      ;; catch-app-errors)
                      (pushnew err *evcb-err*))))
        ;; run the event handler
        (apply event-cb args))
      (apply event-cb args)))

