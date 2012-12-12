;;; This shows an example echo server that writes everything it receives on port
;;; 5000 back to the connected client. A ctrl-c exits the server.

(ql:quickload :cl-async)

(defun echo-server ()
  (as:tcp-server nil 5000
    (lambda (sock data)
      ;; echo data back to client
      (as:write-socket-data sock data))
    (lambda (ev)
      (format t "ev: ~a~%" ev)))
  (as:signal-handler as:+sigint+
    (lambda (sig)
      (declare (ignore sig))
      (as:exit-event-loop))))

(as:start-event-loop #'echo-server)


