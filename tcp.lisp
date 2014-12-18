(in-package :cl-async)

(define-condition tcp-info (streamish-info)
  ((streamish :initarg :socket :accessor tcp-socket :initform nil))
  (:documentation "Base TCP condition. Holds the socket object."))

(define-condition tcp-error (streamish-error tcp-info)
  ()
  (:documentation "Describes a general TCP connection error."))

(define-condition tcp-eof (streamish-eof tcp-error) ()
  (:documentation "Passed to an event callback when a peer closes a TCP connection."))

(define-condition tcp-reset (tcp-error) ()
  (:documentation "Passed to an event callback when a TCP connection times out."))

(define-condition tcp-timeout (tcp-error) ()
  (:documentation "Passed to an event callback when a TCP connection times out."))

(define-condition tcp-refused (tcp-error) ()
  (:documentation "Passed to an event callback when a TCP connection is refused."))

(define-condition tcp-accept-error (tcp-error)
  ((listener :accessor tcp-accept-error-listener :initarg :listener :initform (cffi:null-pointer))
   (tcp-server :accessor tcp-accept-error-tcp-server :initarg :tcp-server :initform nil))
  (:report (lambda (c s) (format s "Error accepting TCP connection: ~a" (tcp-accept-error-listener c))))
  (:documentation "Passed to a TCP server's event-cb when there's an error accepting a connection."))

(define-condition socket-closed (streamish-closed tcp-error) ()
  (:report (lambda (c s) (format s "Closed TCP socket being operated on: ~a." (tcp-socket c))))
  (:documentation "Thrown when a closed socket is being operated on."))

(define-condition tcp-server-bind-error (tcp-error)
  ((addr :accessor tcp-server-bind-error-addr :initarg :addr :initform nil)
   (port :accessor tcp-server-bind-error-port :initarg :port :initform nil))
  (:report (lambda (c s) (format s "Error binding TCP server (~a:~a)"
                                 (tcp-server-bind-error-addr c)
                                 (tcp-server-bind-error-port c))))
  (:documentation "Thrown when a server fails to bind (generally, the port is already in use)."))

(defclass socket (streamish)
  ((c :accessor socket-c)
   (data :accessor socket-data)
   (closed :accessor socket-closed)
   (buffer :accessor socket-buffer :initarg :buffer :initform (make-buffer)
     :documentation "Holds data sent on the socket that hasn't been sent yet.")
   (bufferingp :accessor socket-buffering-p :initform nil
     :documentation "Lets us know if the socket is currently buffering output.")
   (connected :accessor socket-connected :initarg :connected :initform nil)
   (direction :accessor socket-direction :initarg :direction :initform nil)
   (drain-read-buffer :accessor socket-drain-read-buffer))
  (:documentation "Wraps around a socket."))

(defmethod errno-event ((socket socket) (errno (eql (uv:errval :etimedout))))
  (make-instance 'tcp-timeout :socket socket :code errno :msg "connection timed out"))

(defmethod errno-event ((socket socket) (errno (eql (uv:errval :econnreset))))
  (make-instance 'tcp-reset :socket socket :code errno :msg "connection reset"))

(defmethod errno-event ((socket socket) (errno (eql (uv:errval :eof))))
  (make-instance 'tcp-eof :socket socket))

(defmethod errno-event ((socket socket) (errno (eql (uv:errval :econnrefused))))
  (make-instance 'tcp-refused :socket socket :code errno :msg "connection refused"))

(defclass tcp-server ()
  ((c :accessor tcp-server-c :initarg :c :initform (cffi:null-pointer))
   (closed :accessor tcp-server-closed :initarg :closed :initform nil)
   (stream :accessor tcp-server-stream :initarg :stream :initform nil))
  (:documentation "Wraps around a connection listener."))

(defun socket-closed-p (socket)
  "Return whether a socket is closed or not.
  Same as streamish-closed-p."
  (streamish-closed-p socket))

(defun close-socket (socket &key force)
  "Free a socket (uvstream) and clear out all associated data.
  Same as close-streamish."
  (close-streamish socket :force force))

(defmethod close-streamish :after ((socket socket) &key &allow-other-keys)
  (if (eq (socket-direction socket) :in)
      (decf (event-base-num-connections-in *event-base*))
      (decf (event-base-num-connections-out *event-base*))))

(defgeneric close-tcp-server (socket)
  (:documentation
    "Closes a TCP server. If already closed, does nothing."))

(defmethod close-tcp-server ((tcp-server tcp-server))
  (unless (tcp-server-closed tcp-server)
    (setf (tcp-server-closed tcp-server) t)
    (let ((server-c (tcp-server-c tcp-server)))
      ;; force so we don't do shutdown (can't shutdown a server)
      (do-close-streamish server-c :force t))))

(defun set-socket-timeouts (socket read-sec write-sec &key socket-is-uvstream)
  "Set the read/write timeouts on a socket."
  (check-streamish-open socket)
  (let* ((uvstream (if socket-is-uvstream
                       socket
                       (socket-c socket)))
         (read-sec (and read-sec (< 0 read-sec) read-sec))
         (write-sec (and write-sec (< 0 write-sec) write-sec))
         (socket-data (deref-data-from-pointer uvstream))
         (event-cb (getf (get-callbacks uvstream) :event-cb))
         (socket (getf socket-data :streamish))
         (cur-read-timeout (getf (getf socket-data :read-timeout) :event))
         (cur-write-timeout (getf (getf socket-data :write-timeout) :event))
         (read-timeout (when read-sec
                         (delay (lambda () (event-handler (uv:errval :etimedout) event-cb
                                                          :streamish socket))
                                :time read-sec)))
         (write-timeout (when write-sec
                          (delay (lambda () (event-handler (uv:errval :etimedout) event-cb
                                                           :streamish socket))
                                 :time write-sec))))
    ;; clear the timeouts
    (when (and cur-read-timeout (not read-sec) (not (event-freed-p cur-read-timeout)))
      (free-event cur-read-timeout))
    (when (and cur-write-timeout (not write-sec) (not (event-freed-p cur-write-timeout)))
      (free-event cur-write-timeout))
    (when read-timeout
      (setf (getf socket-data :read-timeout) (cons read-timeout read-sec)))
    (when write-timeout
      (setf (getf socket-data :write-timeout) (cons write-timeout write-sec)))
    (attach-data-to-pointer uvstream socket-data)))

(defun enable-socket (socket &key read write)
  "Enable read/write monitoring on a socket. If :read or :write are nil, they
   are not disabled, but rather just not enabled."
  (declare (ignore socket read write))
  (error "not implemented"))

(defun disable-socket (socket &key read write)
  "Disable read/write monitoring on a socket. If :read or :write are nil, they
   are not enabled, but rather just not disabled."
  (declare (ignore socket read write))
  (error "not implemented"))

(defmethod streamish-write ((socket socket) data &key start end force &allow-other-keys)
  ;; if the socket is connected, just send the data out as
  ;; usual. if not connected, buffer the write in the socket's
  ;; write buffer until connected
  (cond ((not (socket-connected socket))
         ;; the socket isn't connected yet. libuv is supposed to
         ;; queue the writes until it connects, but it doesn't
         ;; actually work, so we do our own buffering here. this
         ;; is all flushed out in the tcp-connect-cb.
         (unless (socket-closed-p socket)
           (write-to-buffer (streamish-convert-data data)
                            (socket-buffer socket) start end)))
        ((and (not force) *buffer-writes*)
         ;; buffer the socket data until the next event loop.
         ;; this avoids multiple (unneccesary) calls to uv_write,
         ;; which is fairly slow
         (write-to-buffer (streamish-convert-data data) (socket-buffer socket) start end)
         (unless (socket-buffering-p socket)
           (setf (socket-buffering-p socket) t)
           ;; flush the socket's buffer on the next loop
           (as:with-delay ()
             (unless (socket-closed-p socket)
               (setf (socket-buffering-p socket) nil)
               (write-to-uvstream (socket-c socket)
                                  (buffer-output (socket-buffer socket)) :start start :end end)
               (setf (socket-buffer socket) (make-buffer))))))
        (t
         (call-next-method))))

(defun write-socket-data (socket data &rest args &key &allow-other-keys)
  "An compatibility alias for STREAMISH-WRITE."
  (apply #'streamish-write socket data args))

(defgeneric write-pending-socket-data (socket)
  (:documentation
    "Write any pending data on the given socket to its underlying stream."))

(defmethod write-pending-socket-data ((socket socket))
  (let ((pending (buffer-output (socket-buffer socket))))
    (setf (socket-buffer socket) (make-buffer))
    (write-socket-data socket pending :force t)))

(define-c-callback tcp-connect-cb :void ((req :pointer) (status :int))
  "Called when an outgoing TCP socket connects."
  (let* ((uvstream (deref-data-from-pointer req))
         (stream-data (deref-data-from-pointer uvstream))
         (socket (getf stream-data :streamish))
         (stream (getf stream-data :stream))
         (callbacks (get-callbacks uvstream))
         (event-cb (getf callbacks :event-cb))
         (connect-cb (getf callbacks :connect-cb)))
    (catch-app-errors event-cb
      (unless (zerop status)
        (unless (socket-closed-p socket)
          (run-event-cb 'event-handler status event-cb :streamish socket))
        (return-from tcp-connect-cb))
      (free-pointer-data req :preserve-pointer t)
      (uv:free-req req)
      (setf (socket-connected socket) t)
      (unless (socket-closed-p socket)
        ;; start reading on the socket
        (let ((res (uv:uv-read-start uvstream
                                     (cffi:callback streamish-alloc-cb)
                                     (cffi:callback streamish-read-cb))))
          (if (zerop res)
              (progn
                (when connect-cb
                  (funcall connect-cb (or stream socket)))
                ;; write any buffered output we've stored up
                (write-pending-socket-data socket))
              (run-event-cb 'event-handler res event-cb :streamish socket)))))))

(defun init-incoming-socket (server status)
  "Called by the tcp-accept-cb when an incoming connection is detected. Sets up
   a socket between the client and the server along with any callbacks the
   server has attached to it. Returns the cl-async socket object created."
  (let* ((server-class (deref-data-from-pointer server))
         (callbacks (get-callbacks server))
         (read-cb (getf callbacks :read-cb))
         (event-cb (getf callbacks :event-cb))
         (connect-cb (getf callbacks :connect-cb)))
    (catch-app-errors event-cb
      (if (< status 0)
          ;; error! call the handler
          (run-event-cb 'event-handler status event-cb)
          ;; great, keep going
          (let* ((stream-data-p (tcp-server-stream server-class))
                 (uvstream (let ((s (uv:alloc-handle :tcp)))
                             (uv:uv-tcp-init (event-base-c *event-base*) s)
                             s))
                 (socket (make-instance 'socket :c uvstream
                                                :direction :in
                                                :connected t
                                                :drain-read-buffer (not stream-data-p)))
                 (stream (when stream-data-p (make-instance 'async-io-stream :socket socket))))
            (if (zerop (uv:uv-accept server uvstream))
                (progn
                  (attach-data-to-pointer uvstream (list :streamish socket :stream stream))
                  (save-callbacks uvstream (list :read-cb read-cb :event-cb event-cb))
                  (when connect-cb (funcall connect-cb socket))
                  (uv:uv-read-start uvstream
                                    (cffi:callback streamish-alloc-cb)
                                    (cffi:callback streamish-read-cb)))
                (uv:uv-close uvstream (cffi:null-pointer))))))))

(define-c-callback tcp-accept-cb :void ((server :pointer) (status :int))
  "Called by a listener when an incoming connection is detected. Thin wrapper
   around init-incoming-socket, which does all the setting up of callbacks and
   pointers and so forth."
  (init-incoming-socket server status))

(defun init-tcp-socket (read-cb event-cb &key data stream connect-cb write-cb (read-timeout -1) (write-timeout -1) (dont-drain-read-buffer nil dont-drain-read-buffer-supplied-p))
  "Initialize an async socket, but do not connect it."
  (check-event-loop-running)

  (let* ((uvstream (uv:alloc-handle :tcp))
         ;; assume dont-drain-read-buffer if unspecified and requesting a stream
         (dont-drain-read-buffer (if (and stream (not dont-drain-read-buffer-supplied-p))
                                     t
                                     dont-drain-read-buffer))
         (socket (make-instance 'socket :c uvstream
                                        :direction :out
                                        :drain-read-buffer (not dont-drain-read-buffer)))
         (tcp-stream (when stream (make-instance 'async-io-stream :socket socket))))
    (uv:uv-tcp-init (event-base-c *event-base*) uvstream)
    (when data
      (write-socket-data socket data))
    (save-callbacks uvstream (list :read-cb read-cb
                                   :event-cb event-cb
                                   :write-cb write-cb
                                   :connect-cb connect-cb))
    ;; allow the socket/stream class to be referenced directly by the uvstream
    (attach-data-to-pointer uvstream (list :streamish socket
                                           :stream tcp-stream))
    ;; call this AFTER attach-data-to-pointer because this appends to the data
    (set-socket-timeouts uvstream read-timeout write-timeout :socket-is-uvstream t)
    (if stream
        tcp-stream
        socket)))

(defun connect-tcp-socket (socket/stream host port &key event-cb)
  "Connect a tcp socket initialized with init-tcp-socket."
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
                 (uv:uv-tcp-connect req uvstream sockaddr (cffi:callback tcp-connect-cb))))))
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
  (let ((socket/stream (apply #'init-tcp-socket
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
                       (uv:uv-listen server-c backlog (cffi:callback tcp-accept-cb)))))
      ;; check that our listener instantiated properly
      (when (or (< r-bind 0)
                (< r-listen 0))
        (close-tcp-server server-instance)
        (event-handler (or r-listen r-bind) event-cb :catch-errors t)
        (return-from tcp-server))
      ;; make sure the server is closed/freed on exit
      (add-event-loop-exit-callback (lambda ()
                                      (close-tcp-server server-instance)))
      (attach-data-to-pointer server-c server-instance)
      ;; setup an accept error cb
      (save-callbacks server-c (list :read-cb read-cb :event-cb event-cb :connect-cb connect-cb))
      ;; return the listener, which can be closed by the app if needed
      server-instance)))
