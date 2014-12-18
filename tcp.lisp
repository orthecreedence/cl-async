(in-package :cl-async)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (find-class 'tcp-info) (find-class 'socket-info))
  (setf (find-class 'tcp-error) (find-class 'socket-error))
  (setf (find-class 'tcp-reset) (find-class 'socket-reset))
  (setf (find-class 'tcp-timeout) (find-class 'socket-timeout))
  (setf (find-class 'tcp-refused) (find-class 'socket-refused))
  (setf (find-class 'tcp-accept-error) (find-class 'socket-accept-error)))

;; TBD: tcp-server-bind-error is not actually used currently
(define-condition tcp-server-bind-error (tcp-error)
  ((addr :accessor tcp-server-bind-error-addr :initarg :addr :initform nil)
   (port :accessor tcp-server-bind-error-port :initarg :port :initform nil))
  (:report (lambda (c s) (format s "Error binding TCP server (~a:~a)"
                                 (tcp-server-bind-error-addr c)
                                 (tcp-server-bind-error-port c))))
  (:documentation "Thrown when a server fails to bind (generally, the port is already in use)."))

(defclass tcp-socket (socket) ())
(defclass tcp-server (socket-server) ())

(defmethod server-socket-class ((server tcp-server)) 'tcp-socket)

(defmethod make-socket-handle ((socket tcp-socket))
  (let ((s (uv:alloc-handle :tcp)))
    (uv:uv-tcp-init (event-base-c *event-base*) s)
    s))

(defun connect-tcp-socket (socket/stream host port &key event-cb)
  "Connect a tcp socket initialized with init-client-socket."
  (let* ((socket (if (subtypep (type-of socket/stream) 'async-stream)
                     (stream-socket socket/stream)
                     socket/stream))
         (uvstream (socket-c socket)))
    ;; track the connection
    (incf (event-base-num-connections-out *event-base*))
    ;; only connect if we didn't get an existing fd passed in
    (flet ((do-connect (ip port)
             (with-ip-to-sockaddr ((sockaddr) ip port)
               (let ((req (uv:alloc-req :connect)))
                 ;; make sure we can grab the original uvstream from the req
                 (attach-data-to-pointer req uvstream)
                 (uv:uv-tcp-connect req uvstream sockaddr (cffi:callback socket-connect-cb))))))
      (if (ip-address-p host)
          ;; got an IP so just connect directly
          (do-connect host port)

          ;; get a DNS base and do an async lookup
          (dns-lookup
            host
            (lambda (ip family)
              (declare (ignore family))
              (do-connect ip port))
            event-cb
            :family +af-inet+))))
  socket/stream)

(defun tcp-connect (host port read-cb event-cb &key data stream connect-cb write-cb (read-timeout -1) (write-timeout -1) (dont-drain-read-buffer nil dont-drain-read-buffer-supplied-p))
  "Open a TCP connection asynchronously. Optionally send data out once connected
   via the :data keyword (can be a string or byte array)."
  (check-type data (or null (simple-array octet (*)) string))
  (let ((socket/stream (apply #'init-client-socket
                              'tcp-socket
                              (append (list read-cb event-cb
                                            :data data
                                            :stream stream
                                            :connect-cb connect-cb
                                            :write-cb write-cb
                                            :read-timeout read-timeout
                                            :write-timeout write-timeout)
                                      (when dont-drain-read-buffer-supplied-p
                                        (list :dont-drain-read-buffer dont-drain-read-buffer))))))
    (connect-tcp-socket socket/stream host port :event-cb event-cb)
    socket/stream))

(defun tcp-server (bind-address port read-cb event-cb &key connect-cb backlog stream fd)
  "Start a TCP listener on the current event loop. Returns a tcp-server class
   which can be closed with close-tcp-server"
  (check-event-loop-running)
  (let ((server-c (uv:alloc-handle :tcp)))
    (uv:uv-tcp-init (event-base-c *event-base*) server-c)
    (let* ((r-bind (if fd
                       (uv:uv-tcp-open server-c fd)
                       (with-ip-to-sockaddr ((sockaddr) bind-address port)
                         (uv:uv-tcp-bind server-c sockaddr 0))))
           (server-instance (make-instance 'tcp-server
                                           :c server-c
                                           :stream stream))
           (backlog (if (or (null backlog)
                            (< backlog 0))
                        128
                        backlog))
           (r-listen (when (zerop r-bind)
                       (uv:uv-listen server-c backlog (cffi:callback socket-accept-cb)))))
      ;; check that our listener instantiated properly
      (when (or (< r-bind 0)
                (< r-listen 0))
        (close-socket-server server-instance)
        (event-handler (or r-listen r-bind) event-cb :catch-errors t)
        (return-from tcp-server))
      ;; make sure the server is closed/freed on exit
      (add-event-loop-exit-callback (lambda ()
                                      (close-socket-server server-instance)))
      (attach-data-to-pointer server-c server-instance)
      ;; setup an accept error cb
      (save-callbacks server-c (list :read-cb read-cb :event-cb event-cb :connect-cb connect-cb))
      ;; return the listener, which can be closed by the app if needed
      server-instance)))

;; compatiblity funcs
(defun close-tcp-server (server)
  (close-socket-server server))

(defun tcp-server-closed (server)
  (socket-server-closed server))
