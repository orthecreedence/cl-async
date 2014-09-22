(in-package :cl-async-test)
(in-suite cl-async-test-core)

(test delay-simple
  "Test a simple delay"
  (is (eq (async-let ((ran nil))
            (as:delay (lambda () (setf ran :lol))))
          :lol)))

(test delay-timer
  "Test the time accuracy of delay"
  (multiple-value-bind (start end)
      (async-let ((start (get-internal-real-time))
                  (end :blank))
        (as:delay (lambda () (setf end (get-internal-real-time)))
                  :time 2))
    (is (<= 1.98 (/ (- end start) internal-time-units-per-second) 2.02))))

(test delay-multi
  "Test multiple timers"
  (multiple-value-bind (timer1 timer2 timer3)
      (async-let ((timer1 nil)
                  (timer2 nil)
                  (timer3 nil))
        (as:delay (lambda () (setf timer1 t)) :time 1)
        (as:delay (lambda () (setf timer2 t)) :time 1)
        (as:delay (lambda () (setf timer3 t)) :time 1))
    (is (identity timer1))
    (is (identity timer2))
    (is (identity timer3))))

(test interval
  "Test intervals"
  (is (eq (async-let ((c 0))
            (let ((interval (as:interval (lambda () (incf c)) :time .1)))
              (as:with-delay (.32)
                (as:remove-interval interval))))
          3)))

