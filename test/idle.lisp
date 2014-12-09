(in-package :cl-async-test)
(in-suite cl-async-test-core)

(test idle
  "Test idling."
  (multiple-value-bind (count)
      (async-let ((count 0))
        (test-timeout 5)
        (let* ((idler nil)
               (idle-cb (lambda ()
                          (incf count)
                          (when (<= 517 count)
                            (as:free-idler idler)))))
          (setf idler (as:idle idle-cb))))
    (is (= count 517))))

