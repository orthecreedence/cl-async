(in-package :cl-async-test)
(in-suite cl-async-test-core)

;; TODO: test non-exit on events existing

(test event-loop-starts
  "Test that the event loop actually starts"
  (is (eq (async-let ((started nil))
           (setf started t))
          t)))

(test event-loop-exit
  "Test that an event loop can be exited (unnaturally)"
  (is (eq (async-let ((delayed :no))
            (as:delay (lambda () (setf delayed :yes))
                      :time 1)
            (as:exit-event-loop))
          :no)))

(test catch-app-errors
  "Test the global event handler works appropriately"
  (is (subtypep
        (let ((err nil))
          (as:start-event-loop
            (lambda ()
              (error "Test"))
            :catch-app-errors t
            :default-event-cb
              (lambda (ev)
                (setf err ev)))
            (type-of err))
        'error)))

(test data-and-fn-pointers
  "Test for the correct number of data/function pointers for a set of operations"
  (multiple-value-bind (data-pt fn-pt)
      (async-let ((data-pt nil)
                  (fn-pt nil))
        (as:delay (lambda ()) :time 2)
        (as:delay (lambda ()) :time 2)
        (as:delay (lambda ()) :time 2)
        (let ((stats (as:stats)))
          (setf data-pt (getf stats :data-registry-count))
          (setf fn-pt (getf stats :fn-registry-count))))
    (is (= data-pt 4))
    (is (= fn-pt 5))))

(test exit-callbacks
  "Test that functions assigned to be called when event loop exits are run"
  (multiple-value-bind (yes-ran)
      (async-let ((yes-ran nil))
        (add-event-loop-exit-callback
          (lambda ()
            (setf yes-ran :omglolwtf)))
        (as:delay (lambda () nil) :time .2))
    (is (eq yes-ran :omglolwtf))))
