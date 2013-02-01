(in-package :cl-async-test)

(defparameter *http-response*
  (babel:string-to-octets
    (with-output-to-string (s)
      (format s "HTTP/1.1 200 OK~c~c" #\return #\newline)
      (format s "Date: Wed, 03 Oct 2012 23:43:10 GMT~c~c" #\return #\newline)
      (format s "Content-Type: text/plain~c~c" #\return #\newline)
      (format s "Content-Length: 9~c~c" #\return #\newline)
      (format s "~c~c" #\return #\newline)
      (format s "omglolwtf"))))

(defun tcp-benchmark-client (&key (num-requests 40000) (delay 1) (client-id 0))
  (as:tcp-connect "127.0.0.1" 9009
    (lambda (sock data)
      (declare (ignore sock data)))
    (lambda (e)
      (unless (subtypep (type-of e) 'as:tcp-eof)
        (format t "(client event) ~a~%" e)))
    :data (format nil "GET /~c~c~c~c" #\return #\newline #\return #\newline))
  (when (< (1+ client-id) num-requests)
    (as:delay
      (lambda ()
        (tcp-benchmark-client :num-requests num-requests
                              :delay delay
                              :client-id (1+ client-id)))
      :time delay)))

(defun tcp-benchmark-server (&key (num-requests 40000) (request-delay 0))
  (let ((server nil)
        (finished-requests 0)
        (last-finished 0)
        (last-time 0))
    (setf server
          (as:tcp-server nil 9009
            (lambda (socket data)
              (declare (ignore data))
              (flet ((delay-fn ()
                       (unless (as:socket-closed-p socket)
                         (as:write-socket-data
                           socket *http-response*
                           :write-cb (lambda (socket)
                                       (as:close-socket socket)
                                       (incf finished-requests)
                                       (when (<= num-requests finished-requests)
                                         (as:close-tcp-server server)))))))
                (if (< 0 request-delay)
                    (as:delay #'delay-fn :time request-delay)
                    (funcall #'delay-fn))))
            (lambda (err)
              (format t "TCP server event: ~a~%" err))))
    (labels ((show-stats ()
               (let* ((stats (as:stats))
                      (incoming (getf stats :incoming-tcp-connections))
                      (outgoing (getf stats :outgoing-tcp-connections))
                      (now (get-internal-real-time))
                      (sec (/ (- now last-time) internal-time-units-per-second))
                      (rate (/ (- finished-requests last-finished) sec)))
                 (setf last-finished finished-requests
                       last-time now)
                 (format t "incoming: ~a~%outgoing: ~a~%finished: ~a~%rate: ~f req/s~%~%" incoming outgoing finished-requests rate)
                 ;(room)
                 (format t "---------------~%~%"))
               (unless (as::tcp-server-closed server)
                 (as:delay #'show-stats :time 2))))
      (show-stats))))

(defun tcp-benchmark ()
  (as:start-event-loop
    (lambda ()
      (format t "Starting TCP benchmark. This might take a bit.~%")
      (tcp-benchmark-server :num-requests 40000 :request-delay 0)
      (tcp-benchmark-client :num-requests 40000 :delay 0)))
  (format t "~%---~%TCP Benchmark finished.~%"))
