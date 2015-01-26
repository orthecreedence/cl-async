(defpackage :cl-async-repl
  (:use :cl)
  (:nicknames :as-repl)
  (:export #:event-thread-running-p
           #:start-async-repl
           #:ensure-async-repl
           #:stop-async-repl))

(in-package :cl-async-repl)

(defparameter *globals*
  '(*debug-io* *query-io* *terminal-io* *standard-output*
    *standard-input* *error-output* *trace-output*
    *print-array* *print-base* *print-radix*
    *print-case* *print-circle* *print-escape*
    *print-gensym* *print-level* *print-length*
    *print-lines* *print-miser-width* *print-pretty*
    *print-readably* *print-right-margin*
    *package*)
  "Special variables whose values are copied to the event thread.")

(defvar *event-thread* nil
  "Current event thread, or NIL if event thread isn't running.")
(defvar *notifier* nil
  "Notifier object that is used to execute actions in the event thread")
(defvar *lock* (bt:make-lock)
  "Action queue mutex.")
(defvar *pending-actions* '()
  "The list of pending actions to be executed inside the event thread.")
(defvar *sync-action-done* (bt:make-condition-variable)
  "Condition variable used for synchronous actions in the event thread.")
(defvar *sync-action-lock* (bt:make-lock)
  "Mutex used synchronous actions in the event thread.")

(defun wrap-action (action)
  "Wrap ACTION (a function) by establishing ABORT-CALLBACK and EXIT-EVENT-LOOP restarts."
  #'(lambda ()
      (cl-async-util:call-with-callback-restarts
        action
        :abort-restart-description "Abort the current action.")))

(defun schedule-action (action)
  "Asynchronously schedule the action to be executed in the
  event thread. ACTION should be a function."
  (bt:with-lock-held (*lock*)
    (assert *event-thread* () "event thread not started")
    (setf *pending-actions*
          (nconc *pending-actions* (list action))))
  (as:trigger-notifier *notifier*)
  (values))

(defun stop-event-thread ()
  "Terminate event thread"
  (schedule-action #'as:exit-event-loop)
  (values))

(defun start-event-thread (&key exit-callback on-startup)
  "Start event thread. ON-STARTUP is called in the event loop thread
  after it's started. EXIT-CALLBACK is called upon the event loop
  termination."
  (assert (not *event-thread*) () "event thread already started")
  (let ((global-values (mapcar #'symbol-value *globals*))
        (loop-ready-lock (bt:make-lock))
        (loop-ready (bt:make-condition-variable)))
    (setf *event-thread*
          (bt:make-thread
           #'(lambda ()
               (loop for var in *globals*
                     for value in global-values
                     do (setf (symbol-value var) value))
               (unwind-protect
                    (as:with-event-loop ()
                      (as:add-event-loop-exit-callback
                       (lambda ()
                         (when exit-callback
                           (funcall exit-callback))
                         (bt:with-lock-held (*lock*)
                           (unless (as:notifier-freed-p *notifier*)
                             (as:free-notifier *notifier*))
                           (setf *event-thread* nil
                                 *notifier* nil
                                 *pending-actions* '()))))
                      (format *debug-io* "~&;; event thread started.~%")
                      (bt:with-lock-held (loop-ready-lock)
                        (setf *notifier* (as:make-notifier
                                          #'(lambda ()
                                              (let ((actions
                                                      (bt:with-lock-held (*lock*)
                                                        (shiftf *pending-actions* '()))))
                                                (mapc #'funcall actions)))
                                          :single-shot nil))
                        (bt:condition-notify loop-ready))
                      (when on-startup
                        (funcall on-startup)))
                 (format *debug-io* "~&;; event thread exited.~%")))
           :name "event-thread"))
    (bt:with-lock-held (loop-ready-lock)
      (loop until *notifier*
            do (bt:condition-wait loop-ready loop-ready-lock))))
  (values))

(defun sync-action (action)
  "Perform an action synchronously in the event thread, returning
  its result. ACTION should be a function; multiple return values
  are supported."
  (let ((result nil)
        (done-p nil))
    (setf action (wrap-action action))
    (schedule-action
     #'(lambda ()
         (setf result (multiple-value-list (funcall action)))
         (bt:with-lock-held (*sync-action-lock*)
           (setf done-p t)
           (bt:condition-notify *sync-action-done*))))
    (bt:with-lock-held (*sync-action-lock*)
      (loop until done-p
            do (bt:condition-wait *sync-action-done* *sync-action-lock*)))
    (apply #'values result)))

(defun sync-eval (form)
  "Evaluate the form synchronously in the event thread, returning
  its result."
  (let ((package *package*))
    (multiple-value-prog1
        (sync-action #'(lambda ()
                         (unwind-protect
                              (eval form)
                           (setf package *package*))))
      (setf *package* package))))

(defun repl-hook-sym ()
  (assert (find-package "SWANK") () "SWANK package not found")
  (or (find-symbol "*SLIME-REPL-EVAL-HOOKS*" "SWANK") ()
      (error "Cannot initialize *SLIME-REPL-EVAL-HOOKS*, ~
                      use (eval-in-gui-thread ...) form.")))

(defun install-repl-hook (func)
  "Install FUNC as SLIME REPL hook."
  (pushnew func (symbol-value (repl-hook-sym))))

(defun uninstall-repl-hook (func)
  "Remove FUNC from the list of SLIME REPL hooks."
  (setf (symbol-value (repl-hook-sym))
        (delete func (symbol-value (repl-hook-sym)))))

(defun event-thread-running-p ()
  "Returns true if the event thread used by async REPL is running"
  (bt:with-lock-held (*lock*)
    (when *event-thread* t)))

(defun start-async-repl (&optional on-startup)
  "Start event thread and install SLIME REPL hook so that everything is
  evaluated in that thread. Stopping the event loop via (as:stop-event-thread)
  removes the hook.
  If ON-STARTUP function is specified, it's executed in the event loop
  thread after it's started.

  Sets *safe-sldb-quit-restart* to true."
  (start-event-thread :on-startup on-startup
                      :exit-callback #'(lambda () (uninstall-repl-hook 'sync-eval)))
  (install-repl-hook 'sync-eval)
  (setf cl-async-base:*safe-sldb-quit-restart* t)
  (values))

(defun ensure-async-repl (&optional on-startup)
  "If event loop is not started, calls START-ASYNC-REPL
  passing ON-STARTUP to it. If it's already started,
  calls ON-STARTUP if it's not null."
  (cond ((not (event-thread-running-p))
         (start-async-repl on-startup))
        (on-startup
         (funcall on-startup))))

(defun stop-async-repl ()
  "Stop event thread and uninstall SLIME REPL hook.
  Sets *safe-sldb-quit-restart* to false."
  (stop-event-thread)
  (setf cl-async-base:*safe-sldb-quit-restart* nil))
