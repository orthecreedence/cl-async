;;; Spawn 100 subprocesses and wait for them all to wake. Implementation of
;;; sleep sort.

(ql:quickload :cl-async :silent t)
(ql:quickload :babel :silent t)

(defun spawn-one (v)
  (let ((sh (format NIL "sleep ~f; echo ~:*~f~%" v)))
    (as:spawn "bash" `("-c" ,sh)
              :output
              (list :pipe
                    :read-cb
                    (lambda (pipe data)
                      (format T "~A~&" (babel:octets-to-string data)))))))

(as:with-event-loop (:catch-app-errors t)
  (let ((ar (loop for i below 100 collect (/ (random 100) 10))))
    (mapc #'spawn-one ar)))
