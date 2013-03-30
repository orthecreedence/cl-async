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

(defun benchmark-server (&key (port 9009) (request-delay 0) (num-requests 40000))
  (as:start-event-loop
    (lambda ()
      (let ((server nil)
            (finished-requests 0)
            (last-finished 0)
            (last-time 0))
        (labels ((show-stats ()
                   (let* ((stats (as:stats))
                          (incoming (getf stats :incoming-tcp-connections))
                          (outgoing (getf stats :outgoing-tcp-connections))
                          (now (get-internal-real-time))
                          (sec (/ (- now last-time) internal-time-units-per-second))
                          (rate (/ (- finished-requests last-finished) sec)))
                     (setf last-finished finished-requests
                           last-time now)
                     (format t "incoming: ~a~%outgoing: ~a~%finished: ~a / ~a~%rate: ~f req/s~%~%" incoming outgoing finished-requests num-requests rate)
                     ;(room)
                     (format t "---------------~%~%"))
                   (unless (as::tcp-server-closed server)
                     (as:delay #'show-stats :time 2)))
                 (read-cb (socket data)
                   (declare (ignore data))
                   (flet ((delay-fn ()
                            (unless (as:socket-closed-p socket)
                              (as:write-socket-data
                                socket *http-response*
                                :write-cb (lambda (socket)
                                            (as:close-socket socket)
                                            (incf finished-requests)
                                            (when (<= num-requests finished-requests)
                                              (as:close-tcp-server server)
                                              (as:free-signal-handler 2)))))))
                     (if (< 0 request-delay)
                         (as:delay #'delay-fn :time request-delay)
                         (funcall #'delay-fn)))))
          (setf server (as:tcp-server nil port
                         #'read-cb
                         (lambda (err)
                           (format t "(benchmark server): ~a~%" err))))
          (as:signal-handler 2
            (lambda (signo)
              (declare (ignore signo))
              (as:close-tcp-server server)))
          (show-stats))))))

(defun benchmark-client (&key (server "127.0.0.1") (port 9009) (num-requests 40000) (delay 1) (client-id 0))
  (as:start-event-loop
    (lambda ()
      (labels ((do-client (client-id)
                 (as:tcp-connect server port
                   (lambda (sock data)
                     (declare (ignore sock data)))
                   (lambda (e)
                     (unless (subtypep (type-of e) 'as:tcp-eof)
                       (format t "(benchmark client): ~a~%" e)))
                   :data (format nil "GET /~c~c~c~c" #\return #\newline #\return #\newline))
                 (when (< (1+ client-id) num-requests)
                   (as:delay
                     (lambda ()
                       (do-client (1+ client-id)))
                     :time delay))))
        (do-client client-id)))))

