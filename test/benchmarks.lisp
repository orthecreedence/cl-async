(in-package :cl-async-test)

;; profiling shortcuts
#+sbcl
(defun setup-profile ()
  (sb-profile:reset)
  (sb-profile:profile
    as::start-event-loop
    as::write-to-evbuffer
    as::drain-evbuffer
    as::socket-drain-read-buffer
    as::init-incoming-socket
    as::set-socket-timeouts
    as::check-socket-open
    as::add-event
    as::free-event
    as::delay
    as::check-event-unfreed
    cl-async-util::attach-data-to-pointer
    cl-async-util::deref-data-from-pointer
    cl-async-util::free-pointer-data
    cl-async-util::clear-callbacks
    cl-async-util::clear-pointer-data
    cl-async-util::get-callbacks
    cl-async-util::save-callbacks
    cl-async-util::split-usec-time
    cl-async-util::create-data-pointer
    cl-async-util::make-pointer-eql-able
    cl-async-util::append-array
    cl-async-util::get-free-timeval
    cl-async-util::release-timeval))

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
                          (fn-count (getf stats :fn-registry-count))
                          (data-count (getf stats :data-registry-count))
                          (now (get-internal-real-time))
                          (sec (/ (- now last-time) internal-time-units-per-second))
                          (rate (/ (- finished-requests last-finished) sec)))
                     (setf last-finished finished-requests
                           last-time now)
                     (format t "fn/data: ~a/~a~%incoming: ~a~%outgoing: ~a~%finished: ~a / ~a~%rate: ~f req/s~%" fn-count data-count incoming outgoing finished-requests num-requests rate)
                     ;(room)
                     (format t "---------------~%"))
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
              (format t "Got sigint, closing server.~%")
              (as:exit-event-loop)))
          (show-stats))))
    :catch-app-errors t))

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

(defun benchmark-delays (&key (num-delays 40000) (delay .0000000000001))
  (let ((delay-num 0))
    (time
      (as:start-event-loop
        (lambda ()
          (labels ((do-delay ()
                     (when (< delay-num num-delays)
                       (incf delay-num)
                       (as:delay #'do-delay :time delay))))
            (do-delay)))))))

(defun benchmark-data-pointers (&key (num-pointers 10000))
  (as:start-event-loop
    (lambda ()
      (time
        (let ((pointers (make-array num-pointers))
              (sink1 nil)
              (sink2 nil))
          (dotimes (i num-pointers)
            (setf (aref pointers i) (cl-async-util::create-data-pointer))
            (let ((pt (aref pointers i))
                  (obj (random 9999999))
                  (cb (list :read (lambda () (format t "omg!~%")) :write (lambda () (format t "lol~%")))))
              (cl-async-util::attach-data-to-pointer pt obj)
              (cl-async-util::save-callbacks pt cb)))
          (dotimes (i 999999)
            (let* ((idx (random num-pointers))
                   (pt (aref pointers idx)))
              (setf sink1 (deref-data-from-pointer pt)
                    sink2 (get-callbacks pt))))
          (loop for pointer across pointers do
            (cl-async-util::free-pointer-data pointer)))))))

(defun lol () (format t "hai...~%"))
(defun simple-test ()
  (as:start-event-loop
    (lambda ()
      (as:delay 'lol :time 1))))
