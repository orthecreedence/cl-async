(in-package :cl-async-test)
(in-suite cl-async-test-core)

(test spawn-simple
  (let ((cb-count 0)
        (exit-status nil)
        (term-signal nil))
    (as:with-event-loop (:catch-app-errors t)
      (test-timeout 3)
      (with-path-under-tmpdir (path "blabla")
        (let (process)
          (setf process
                (as:spawn "touch" (list (namestring path))
                           :exit-cb #'(lambda (act-process act-exit-status act-term-signal)
                                        (incf cb-count)
                                        (is-true (uiop:file-exists-p path))
                                        (is (eq process act-process))
                                        (setf exit-status act-exit-status
                                              term-signal act-term-signal)))))))
    (is (= 1 cb-count))
    (is (= 0 exit-status))
    (is (= 0 term-signal))))

#++
(defun tst ()
  (multiple-value-bind (process stdin)
      (spawn "bash" '("-c" "cat 1>&2")
             :input (list :pipe :data "hello")
             :error-output (list :pipe
                                 :read-cb #'(lambda (p r)
                                              (:printv r)
                                              (:printv p (babel:octets-to-string r)))))
    (declare (ignore process))
    (streamish-write stdin (babel:string-to-octets (format nil "abc~%")))
    (close-streamish stdin)))

;; TBD: input+output redir, input+error output redir
;; TBD: exec failure
;; TBD: kill
