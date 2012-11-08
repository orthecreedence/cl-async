;;; This shows a quick example of using TCP to communicate with a server. In
;;; this case, we'll use HTTP as an example as it's a fairly straightforward
;;; protocol

(ql:quickload :cl-async)

(defun get-http-response (host &optional (port 80))
  (as:tcp-send host port (format nil "GET / HTTP/1.1~c~cHost: ~a~c~c~c~c"
                                 #\return #\newline
                                 host
                                 #\return #\newline
                                 #\return #\newline)
    (lambda (sock data)
      (unless (as:socket-closed-p sock)
        (as:close-socket sock))
      (format t "~a" (babel:octets-to-string data)))
    (lambda (ev)
      (format t "ev: ~a~%" ev))
    :read-timeout 5))

(as:start-event-loop (lambda () (get-http-response "www.google.com")))
