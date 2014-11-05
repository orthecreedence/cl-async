(in-package :cl-async)

(define-condition tcp-info (event-info)
  ((socket :initarg :socket :accessor tcp-socket :initform nil))
  (:report (lambda (c s)
             (print-unreadable-object (c s :type t :identity t)
               (format s "~a" (tcp-socket c)))))
  (:documentation "Base TCP condition. Holds the socket object."))

(define-condition tcp-error (event-error tcp-info) ()
  (:report (lambda (c s)
             (print-unreadable-object (c s :type t :identity t)
               (format s "~a: ~a: ~a" (tcp-socket c) (event-errcode c) (event-errmsg c)))))
  (:documentation "Describes a general TCP connection error."))

(define-condition tcp-eof (tcp-info) ()
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

(define-condition socket-closed (tcp-error) ()
  (:report (lambda (c s) (format s "Closed TCP socket being operated on: ~a." (tcp-socket c))))
  (:documentation "Thrown when a closed socket is being operated on."))

(define-condition tcp-server-bind-error (tcp-error)
  ((addr :accessor tcp-server-bind-error-addr :initarg :addr :initform nil)
   (port :accessor tcp-server-bind-error-port :initarg :port :initform nil))
  (:report (lambda (c s) (format s "Error binding TCP server (~a:~a)"
                                 (tcp-server-bind-error-addr c)
                                 (tcp-server-bind-error-port c))))
  (:documentation "Thrown when a server fails to bind (generally, the port is already in use)."))

(defclass socket ()
  ((c :accessor socket-c :initarg :c :initform (cffi:null-pointer))
   (data :accessor socket-data :initarg data :initform nil
     :documentation "Used to store arbitrary (app-defined) data with a socket.")
   (buffer :accessor socket-buffer :initarg :buffer :initform (make-buffer)
     :documentation "Holds data sent on the socket before it's connected.")
   (connected :accessor socket-connected :initarg :connected :initform nil)
   (closed :accessor socket-closed :initarg :closed :initform nil)
   (closing :accessor socket-closing :initform nil)
   (direction :accessor socket-direction :initarg :direction :initform nil)
   (drain-read-buffer :accessor socket-drain-read-buffer :initarg :drain-read-buffer :initform t))
  (:documentation "Wraps around a libevent bufferevent socket."))

(defclass tcp-server ()
  ((c :accessor tcp-server-c :initarg :c :initform (cffi:null-pointer))
   (closed :accessor tcp-server-closed :initarg :closed :initform nil)
   (stream :accessor tcp-server-stream :initarg :stream :initform nil))
  (:documentation "Wraps around a libevent connection listener."))

(defun get-last-tcp-err ()
  "Since libevent provides a macro but not a function for getting the last error
   code, we have to essentially rebuild that macro here.

   NOTE: As libevent says, this function cannot be considered idempotent. In
   other words, it can become hideously turgid whe...what's that? OH IDEMpotent,
   that makes more sense. Yes, calling it more than once may result in different
   values returned. So, call once and save the result until the next time an
   error happens."
  #+(or :windows :win32 :win64)
    (let ((code (cffi:foreign-funcall "WSAGetLastError" :int)))
      (values code
              (cffi:foreign-funcall "evutil_socket_error_to_string" :int code :string)))
  #-(or :windows :win32 :win64)
    (let ((code (cffi:mem-aref (cffi:foreign-symbol-pointer "errno") :int)))
      (values code
              (cffi:foreign-funcall "strerror" :int code :string))))

(defun check-socket-open (socket)
  "Throw a socket-closed condition if given a socket that's closed."
  (when (typep socket 'socket)
    (when (or (socket-closed socket)
              (socket-closing socket))
      (error 'socket-closed :code -1 :msg "Trying to operate on a closed socket" :socket socket))))

(defun socket-closed-p (socket)
  "Return whether a socket is closed or not."
  (socket-closed socket))

(defgeneric close-socket (socket &key &allow-other-keys)
  (:documentation
    "Free a socket (uvstream) and clear out all associated data."))

(defun do-close-tcp (uvstream &key force)
  "Close a tcp UV stream."
  (uv:uv-read-stop uvstream)
  (cond ((or force (zerop (uv:uv-is-writable uvstream)))
         (uv:uv-close uvstream (cffi:callback tcp-close-cb)))
        (t
         (let* ((shutdown-req (uv:alloc-req :shutdown))
                (r (uv:uv-shutdown shutdown-req uvstream (cffi:callback tcp-shutdown-cb))))
           (if (zerop r)
               (attach-data-to-pointer shutdown-req (list uvstream))
               (uv:uv-close uvstream (cffi:callback tcp-close-cb)))))))

(defmethod close-socket ((socket socket) &key force)
  "Close and free a socket and all of it's underlying structures."
  (check-socket-open socket)
  (setf (socket-closing socket) t)
  (let* ((uvstream (socket-c socket))
         (data (deref-data-from-pointer uvstream))
         (read-timeout (car (getf data :read-timeout)))
         (write-timeout (car (getf data :write-timeout))))
    (dolist (timeout (list read-timeout
                           write-timeout))
      (when (and timeout
                 (not (event-freed-p timeout)))
        (free-event timeout)))
    (if (eq (socket-direction socket) :in)
        (decf (event-base-num-connections-in *event-base*))
        (decf (event-base-num-connections-out *event-base*)))
    (setf (socket-closed socket) t)
    (do-close-tcp uvstream :force force)))

(defgeneric close-tcp-server (socket)
  (:documentation
    "Closes a TCP server. If already closed, does nothing."))

(defmethod close-tcp-server ((tcp-server tcp-server))
  (unless (tcp-server-closed tcp-server)
    (setf (tcp-server-closed tcp-server) t)
    (let ((server-c (tcp-server-c tcp-server)))
      ;; force so we don't do shutdown (can't shutdown a server)
      (do-close-tcp server-c :force t))))

(defun set-socket-timeouts (socket read-sec write-sec &key socket-is-uvstream)
  "Set the read/write timeouts on a socket."
  (check-socket-open socket)
  (let* ((uvstream (if socket-is-uvstream
                       socket
                       (socket-c socket)))
         (read-sec (and read-sec (< 0 read-sec) read-sec))
         (write-sec (and write-sec (< 0 write-sec) write-sec))
         (socket-data (deref-data-from-pointer uvstream))
         (event-cb (getf (get-callbacks uvstream) :event-cb))
         (socket (getf socket-data :socket))
         (cur-read-timeout (getf (getf socket-data :read-timeout) :event))
         (cur-write-timeout (getf (getf socket-data :write-timeout) :event))
         (read-timeout (when read-sec
                         (delay (lambda () (event-handler (uv:errval :etimedout) event-cb :socket socket))
                                :time read-sec)))
         (write-timeout (when write-sec
                          (delay (lambda () (event-handler (uv:errval :etimedout) event-cb :socket socket))
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
  (error "not implemented"))

(defun disable-socket (socket &key read write)
  "Disable read/write monitoring on a socket. If :read or :write are nil, they
   are not enabled, but rather just not disabled."
  (error "not implemented"))

(defun write-to-uvstream (uvstream data)
  "Util function to write data directly to a uv stream object."
  (let* ((data (if (stringp data)
                   (babel:string-to-octets data :encoding :utf-8)
                   data))
         (data-length (length data))
         (data-index 0)
         (buffer-c *output-buffer*)
         (buffer-length (length buffer-c)))
    (loop while (< 0 data-length) do
      (let ((bufsize (min data-length buffer-length)))
        (replace buffer-c data :start2 data-index)
        (let ((req (uv:alloc-req :write))
              (buf (uv:alloc-uv-buf (static-vectors:static-vector-pointer buffer-c) bufsize)))
          (let ((res (uv:uv-write req uvstream buf 1 (cffi:callback tcp-write-cb))))
            (uv:free-uv-buf buf)
            (unless (zerop res)
              (let ((socket (getf (deref-data-from-pointer uvstream) :socket))
                    (event-cb (getf (get-callbacks uvstream) :event-cb)))
                (uv:free-req req)
                (event-handler res event-cb :socket socket)
                (return-from write-to-uvstream)))
            (attach-data-to-pointer req uvstream)
            (decf data-length bufsize)))))))

(defun write-socket-data (socket data &key read-cb write-cb event-cb)
  "Write data into a cl-async socket. Allows specifying read/write/event
   callbacks. Any callback left nil will use that current callback from the
   socket (so they only override when specified, otherwise keep the current
   callback).
   
   Note that libuv doesn't buffer output for non-connected sockets, so we have
   to do it ourselves by checking if the socket is connected and buffering
   accordingly."
  (check-socket-open socket)
  (let* ((uvstream (socket-c socket))
         (write-timeout (getf (deref-data-from-pointer uvstream) :write-timeout))
         (timeout (car write-timeout))
         (do-send (lambda ()
                    ;; if the socket is connected, just send the data out as
                    ;; usual. if not connected, buffer the write in the socket's
                    ;; write buffer until connected
                    (if (socket-connected socket)
                        (write-to-uvstream uvstream data)
                        (let ((bytes (if (stringp data)
                                         (babel:string-to-octets data :encoding :utf-8)
                                         data)))
                          (setf (socket-buffer socket) (append-array (socket-buffer socket) bytes)))))))
    (when write-timeout
      (remove-event timeout)
      (add-event timeout :timeout (cdr write-timeout)))
    (if (or read-cb write-cb event-cb)
        ;; we're specifying callbacks. since we're most likely calling this from
        ;; inside a socket callback and we don't necessarily want to overwrite
        ;; that socket's callbacks until it finishes, we set a delay here so the
        ;; callback binding happens after the caller returns to the event loop.
        (as:delay
          (lambda ()
            (let ((callbacks (get-callbacks uvstream)))
              (save-callbacks uvstream
                              (list :read-cb (or read-cb (getf callbacks :read-cb))
                                    :write-cb (or write-cb (getf callbacks :write-cb))
                                    :event-cb (or event-cb (getf callbacks :event-cb))))
              (funcall do-send))))

        ;; we're not setting callbacks, so just enable the socket and send the
        ;; data
        (funcall do-send))))

(defun write-pending-socket-data (socket)
  "Write any pending data on the given socket to its underlying stream."
  (write-socket-data socket (socket-buffer socket)))

(define-c-callback tcp-alloc-cb :void ((handle :pointer) (size :unsigned-int) (buf :pointer))
  "Called when we want to allocate data to be filled for stream reading."
  (declare (ignore handle))
  (uv:alloc-uv-buf (static-vectors:static-vector-pointer *input-buffer*) (min *buffer-size* size) buf))

(define-c-callback tcp-read-cb :void ((uvstream :pointer) (nread :int) (buf :pointer))
  "Called when a stream has been read into a buffer returned by alloc-cb."
  (let* ((stream-data (deref-data-from-pointer uvstream))
         (read-timeout (getf stream-data :read-timeout))
         (timeout (car read-timeout))
         (socket (getf stream-data :socket))
         (stream (getf stream-data :stream))
         (drain-read (socket-drain-read-buffer socket))
         (callbacks (get-callbacks uvstream))
         (read-cb (getf callbacks :read-cb))
         (event-cb (getf callbacks :event-cb))
         (read-buf (multiple-value-list (uv:uv-buf-read buf)))
         (buf-ptr (car read-buf))
         (buf-len (cadr read-buf)))
    (catch-app-errors event-cb
      (when (< nread 0)
        ;; we got an error
        (run-event-cb 'event-handler nread event-cb :socket socket)
        (return-from tcp-read-cb))

      ;; reset the read timeout
      (when timeout
        (remove-event timeout)
        (add-event timeout :timeout (cdr read-timeout)))
      
      ;; read the buffer
      (let ((bytes (make-array nread :element-type '(unsigned-byte 8))))
        (dotimes (i nread)
          (setf (aref bytes i) (cffi:mem-aref buf-ptr :unsigned-char i)))
        (cond ((and read-cb drain-read)
               ;; we're draining here, so call our read callback
               (funcall read-cb socket bytes))
              (read-cb
               ;; we're not draining and we have a read CB, so stream
               (stream-append-bytes stream bytes)
               (funcall read-cb socket stream)))))))

(define-c-callback tcp-connect-cb :void ((req :pointer) (status :int))
  "Called when an outgoing TCP socket connects."
  (let* ((uvstream (deref-data-from-pointer req))
         (stream-data (deref-data-from-pointer uvstream))
         (socket (getf stream-data :socket))
         (stream (getf stream-data :stream))
         (callbacks (get-callbacks uvstream))
         (event-cb (getf callbacks :event-cb))
         (connect-cb (getf callbacks :connect-cb)))
    (catch-app-errors event-cb
      (unless (zerop status)
        (unless (socket-closed-p socket)
          (run-event-cb 'event-handler status event-cb :socket socket))
        (return-from tcp-connect-cb))
      (free-pointer-data req :preserve-pointer t)
      (uv:free-req req)
      (setf (socket-connected socket) t)
      (unless (socket-closed-p socket)
        ;; start reading on the socket
        (let ((res (uv:uv-read-start uvstream (cffi:callback tcp-alloc-cb) (cffi:callback tcp-read-cb))))
          (if (zerop res)
              (progn
                (when connect-cb
                  (funcall connect-cb (or stream socket)))
                (write-pending-socket-data socket))
              (run-event-cb 'event-handler res event-cb :socket socket)))))))

(define-c-callback tcp-write-cb :void ((req :pointer) (status :int))
  "Called when data is finished being written to a socket."
  (let* ((uvstream (deref-data-from-pointer req))
         (socket (getf (deref-data-from-pointer uvstream) :socket))
         (callbacks (get-callbacks uvstream))
         (write-cb (getf callbacks :write-cb))
         (event-cb (getf callbacks :event-cb)))
    (catch-app-errors event-cb
      (free-pointer-data req :preserve-pointer t)
      (uv:free-req req)
      (if (zerop status)
          (when write-cb
            (funcall write-cb socket))
          (run-event-cb 'event-handler status event-cb :socket socket)))))

(define-c-callback tcp-shutdown-cb :void ((req :pointer) (status :int))
  "Called when a tcp socket shuts down."
  (uv:free-req req)
  (let ((uvstream (car (deref-data-from-pointer req))))
    (when (zerop (uv:uv-is-closing uvstream))
      (uv:uv-close uvstream (cffi:callback tcp-close-cb)))))

(define-c-callback tcp-close-cb :void ((uvstream :pointer))
  "Called when a tcp socket closes."
  ;; !!NOTE!! This callback is used for both tcp clients AND servers! if either
  ;; ever needs special treatment, split out the callbacks
  (free-pointer-data uvstream :preserve-pointer t)
  (uv:free-handle uvstream))

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
                  (attach-data-to-pointer uvstream (list :socket socket :stream stream))
                  (save-callbacks uvstream (list :read-cb read-cb :event-cb event-cb))
                  (when connect-cb (funcall connect-cb socket))
                  (uv:uv-read-start uvstream (cffi:callback tcp-alloc-cb) (cffi:callback tcp-read-cb)))
                (uv:uv-close uvstream (cffi:null-pointer))))))))

(define-c-callback tcp-accept-cb :void ((server :pointer) (status :int))
  "Called by a listener when an incoming connection is detected. Thin wrapper
   around init-incoming-socket, which does all the setting up of callbacks and
   pointers and so forth."
  (init-incoming-socket server status))

(defun init-tcp-socket (read-cb event-cb &key data stream (fd -1) connect-cb write-cb (read-timeout -1) (write-timeout -1) (dont-drain-read-buffer nil dont-drain-read-buffer-supplied-p))
  "Initialize an async socket, but do not connect it."
  (check-event-loop-running)

  (let* ((uvstream (uv:alloc-handle :tcp))
         ;; assume dont-drain-read-buffer if unspecified and requesting a stream
         (dont-drain-read-buffer (if (and stream (not dont-drain-read-buffer-supplied-p))
                                     t
                                     dont-drain-read-buffer))
         (fd (when (<= 0 fd) fd))
         (socket (make-instance 'socket :c uvstream
                                        :direction :out
                                        :drain-read-buffer (not dont-drain-read-buffer)))
         (tcp-stream (when stream (make-instance 'async-io-stream :socket socket))))
    (uv:uv-tcp-init (event-base-c *event-base*) uvstream)
    (when (or data tcp-stream)
      (let* ((old-connect-cb connect-cb)
             (new-connect-cb (lambda (sock)
                               (when old-connect-cb (funcall old-connect-cb sock))
                               (cond ((and data (typep sock 'async-output-stream))
                                      (write-sequence data sock))
                                     (data
                                      (write-socket-data sock data))))))
        (setf connect-cb new-connect-cb)))
    (save-callbacks uvstream (list :read-cb read-cb
                                   :event-cb event-cb
                                   :write-cb write-cb
                                   :connect-cb connect-cb))
    ;; allow the socket/stream class to be referenced directly by the uvstream
    (attach-data-to-pointer uvstream (list :socket socket
                                           :stream tcp-stream))
    ;; call this AFTER attach-data-to-pointer because this appends to the data
    (set-socket-timeouts uvstream read-timeout write-timeout :socket-is-uvstream t)
    ;; wrap existing fd
    (when fd
      (let ((r (uv:uv-tcp-open uvstream fd)))
        (unless (zerop r)
          (event-handler r event-cb :catch-errors t)
          (return-from init-tcp-socket nil))
        (set-socket-nonblocking fd)
        (when (fd-connected-p fd)
          ;; wrapping a pre-connected FD is not allowed in windows =[
          (when (progn #+windows t)
            (error "FD wrapping no longer supported in windows. Please use a poller instead or bug libuv maintainers to support windows wrapping of connected FDs."))
          (let ((req (uv:alloc-req :connect)))
            (attach-data-to-pointer req uvstream)
            (tcp-connect-cb req 0)))))
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

(defun tcp-server (bind-address port read-cb event-cb &key connect-cb backlog stream)
  "Start a TCP listener on the current event loop. Returns a tcp-server class
   which can be closed with close-tcp-server"
  (check-event-loop-running)
  (let ((server-c (uv:alloc-handle :tcp)))
    (with-ip-to-sockaddr ((sockaddr) bind-address port)
      (uv:uv-tcp-init (event-base-c *event-base*) server-c)
      (let* ((r-bind (uv:uv-tcp-bind server-c sockaddr 0))
             (server-class (make-instance 'tcp-server
                                          :c server-c
                                          :stream stream))
             (backlog (if (or (null backlog)
                              (< backlog 0))
                          128
                          backlog))
             (r-listen (uv:uv-listen server-c backlog (cffi:callback tcp-accept-cb))))
        ;; check that our listener instantiated properly
        (when (or (< r-bind 0)
                  (< r-listen 0))
          (uv:uv-close server-c)
          (event-handler (min r-bind r-listen) event-cb :catch-errors t)
          (return-from tcp-server))
        ;; make sure the server is closed/freed on exit
        (add-event-loop-exit-callback (lambda ()
                                        (close-tcp-server server-class)))
        (attach-data-to-pointer server-c server-class)
        ;; setup an accept error cb
        (save-callbacks server-c (list :read-cb read-cb :event-cb event-cb :connect-cb connect-cb))
        ;; return the listener, which can be closed by the app if needed
        server-class))))

