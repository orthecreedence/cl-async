(in-package :cl-async)

;; compatibility
(define-condition-alias tcp-eof socket-eof)
(define-condition-alias tcp-info socket-info)
(define-condition-alias tcp-error socket-error)
(define-condition-alias tcp-reset socket-reset)
(define-condition-alias tcp-timeout socket-timeout)
(define-condition-alias tcp-refused socket-refused)
(define-condition-alias tcp-accept-error socket-accept-error)

;; TBD: tcp-server-bind-error is not actually used currently
(define-condition tcp-server-bind-error (socket-error)
  ((addr :accessor tcp-server-bind-error-addr :initarg :addr :initform nil)
   (port :accessor tcp-server-bind-error-port :initarg :port :initform nil))
  (:report (lambda (c s) (format s "Error binding TCP server (~a:~a)"
                                 (tcp-server-bind-error-addr c)
                                 (tcp-server-bind-error-port c))))
  (:documentation "Thrown when a server fails to bind (generally, the port is already in use)."))

(defclass tcp-mixin () ())
(defclass tcp-socket (tcp-mixin socket)
  ((direction :accessor socket-direction :initarg :direction :initform :out)))
(defclass tcp-server (tcp-mixin socket-server) ())

(defmethod server-socket-class ((server tcp-server)) 'tcp-socket)

(defmethod initialize-instance :after ((socket tcp-socket) &key direction &allow-other-keys)
  (ecase direction
    (:in
     (incf (event-base-num-connections-in *event-base*)))
    (:out
     (incf (event-base-num-connections-out *event-base*)))))

(defmethod close-streamish :after ((socket tcp-socket) &key &allow-other-keys)
  (ecase (socket-direction socket)
    (:in
     (decf (event-base-num-connections-in *event-base*)))
    (:out
     (decf (event-base-num-connections-out *event-base*)))))

(defmethod make-socket-handle ((socket-or-server tcp-mixin))
  (let ((s (uv:alloc-handle :tcp)))
    (uv:uv-tcp-init (event-base-c *event-base*) s)
    s))

(defun connect-tcp-socket (socket/stream host port &key event-cb)
  "Connect a tcp socket initialized with init-client-socket."
  (let* ((socket (if (subtypep (type-of socket/stream) 'async-stream)
                     (streamish socket/stream)
                     socket/stream))
         (uvstream (socket-c socket)))
    ;; only connect if we didn't get an existing fd passed in
    (flet ((do-connect (ip port)
             (with-ip-to-sockaddr ((sockaddr) ip port)
               (let ((req (uv:alloc-req :connect)))
                 ;; make sure we can grab the original uvstream from the req
                 (attach-data-to-pointer req uvstream)
                 (if (zerop (uv:uv-is-closing uvstream))
                     (uv:uv-tcp-connect req uvstream sockaddr (cffi:callback socket-connect-cb))
                     (warn "aborting connection to ~a:~s on a tcp socket that is being closed: ~
                            ~s (uvstream ~s)~%" host port socket/stream uvstream))))))
      (if (ip-address-p host)
          ;; got an IP so just connect directly
          (do-connect host port)

          ;; get a DNS base and do an async lookup
          (dns-lookup
            host
            (lambda (ip family)
              (declare (ignore family))
              (do-connect ip port))
            :event-cb event-cb
            :family +af-inet+))))
  socket/stream)

(defun tcp-connect-new (host port read-cb &key data stream event-cb connect-cb write-cb (read-timeout -1) (write-timeout -1) (dont-drain-read-buffer nil dont-drain-read-buffer-supplied-p))
  "Open a TCP connection asynchronously. Optionally send data out once connected
   via the :data keyword (can be a string or byte array)."
  (check-type data (or null (simple-array octet (*)) string))
  (let* ((socket/stream (apply #'init-client-socket
                               'tcp-socket
                               (append (list :read-cb read-cb
                                             :event-cb event-cb
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

(defun tcp-connect (host port read-cb &rest args)
  "Open a TCP connection asynchronously. Optionally send data out once connected
   via the :data keyword (can be a string or byte array)."
  (let ((event-cb-dep (car args)))
    (unless (or (keywordp event-cb-dep)
                (null event-cb-dep))
      (push :event-cb args)
      (warn "Passing event-cb as the fourth argument to tcp-connect is now deprecated. Please use the :event-cb keyword instead."))
    (apply 'tcp-connect-new
           host port read-cb
           args)))

(defmethod socket-server-bind ((server tcp-server) address fd)
  (destructuring-bind (bind-address port) address
    (if fd
        (uv:uv-tcp-open (socket-server-c server) fd)
        (with-ip-to-sockaddr ((sockaddr) bind-address port)
          (uv:uv-tcp-bind (socket-server-c server) sockaddr 0)))))

(defun tcp-server-new (bind-address port read-cb &key event-cb connect-cb backlog stream fd)
  "Start a TCP listener on the current event loop. Returns a tcp-server class
   which can be closed with close-tcp-server"
  (socket-server 'tcp-server
                 (list bind-address port) read-cb
                 :event-cb event-cb
                 :connect-cb connect-cb
                 :backlog backlog
                 :stream stream
                 :fd fd))

(defun tcp-server (bind-address port read-cb &rest args)
  "Open a TCP connection asynchronously. Optionally send data out once connected
   via the :data keyword (can be a string or byte array)."
  (let ((event-cb-dep (car args)))
    (unless (or (keywordp event-cb-dep)
                (null event-cb-dep))
      (push :event-cb args)
      (warn "Passing event-cb as the fourth argument to tcp-server is now deprecated. Please use the :event-cb keyword instead."))
    (apply 'tcp-server-new
           bind-address port read-cb
           args)))

;; compatiblity funcs
(defun close-tcp-server (server)
  (close-socket-server server))

(defun tcp-server-closed (server)
  (socket-server-closed server))

(defmethod handle-cleanup ((handle-type (eql :tcp)) handle)
  (handle-cleanup :async-socket handle))
