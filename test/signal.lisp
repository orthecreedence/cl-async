(in-package :cl-async-test)
(in-suite cl-async-test-core)

;;; signals are a bitch because
;;;  a) lisp has no native way of sending signals
;;;  b) windows doesn't really support them.
;;;
;;; so, testing signal handlers is going to take some extra thought. in the
;;; meantime, we can make sure the functions at least run...

(test signal-handler-add-remove
  "Test a signal handler adds, and removing it ends the event loop"
  (multiple-value-bind (start end)
      (async-let ((start (get-internal-real-time))
                  (end :blank))
        (as:signal-handler as:+sigint+
          (lambda (sig) (declare (ignore sig))))
        (as:delay (lambda () (as:free-signal-handler as:+sigint+))
                  :time 1))
    (setf end (get-internal-real-time))
    (is (<= 0.98 (/ (- end start) internal-time-units-per-second) 1.02))))
