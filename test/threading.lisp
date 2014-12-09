(in-package :cl-async-test)
(def-suite cl-async-threading-test :in cl-async-test :description "threading tests")
(in-suite cl-async-threading-test)

(test threading-delay
  "Test we can run a delay from outside the event loop."
  (let ((delay-val 0)
        (notifier nil))
    (bt:make-thread
      (lambda ()
        (sleep 2)
        (as:trigger-notifier notifier)))
    (as:with-event-loop ()
      (setf notifier (as:make-notifier
                       (lambda () (incf delay-val 42))))
      (as:with-delay (2)
        (incf delay-val 69)))
    (is (= (+ 42 69) delay-val))))

