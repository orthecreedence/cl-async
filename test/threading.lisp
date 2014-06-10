(in-package :cl-async-test)
(def-suite cl-async-threading-test :in cl-async-test :description "threading tests")
(in-suite cl-async-threading-test)

(test threading-delay
  "Test we can run a delay from outside the event loop."
  (let ((delay-val 0))
    (bt:make-thread
      (lambda ()
        (sleep 1)
        (as:with-threading-context (:io nil)
          (as:with-delay (1)
            (incf delay-val 42)))))
    (as:with-event-loop ()
      (as:with-delay (2)
        (incf delay-val 69)))
    (is (= (+ 42 69) delay-val ))))

(test thread-specific-loop
  "Test thrading from a specific loop."
  (let ((base-id nil)
        (delay-val 0))
    (bt:make-thread
      (lambda ()
        (sleep 1)
        (as:with-threading-context (:base-id base-id)
          (as:with-delay (1)
            (incf delay-val 33)))))
    (as:with-event-loop ()
      (setf base-id (cl-async-base:event-base-id cl-async-base:*event-base*))
      (as:with-delay (2)
        (incf delay-val 77)))
    (is (= (+ 33 77) delay-val))))

