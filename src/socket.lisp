(in-package :cl-async)

(define-condition socket-info (streamish-info)
  ((streamish :initarg :socket :accessor socket :initform nil))
  (:documentation "Base socket condition. Holds the socket object."))

(define-condition socket-error (streamish-error socket-info)
  ()
  (:documentation "Describes a general socket connection error."))

(define-condition socket-eof (streamish-eof socket-error) ()
  (:documentation "Peer closes a socket connection."))

(define-condition socket-reset (socket-error) ()
  (:documentation "Connection reset."))

(define-condition socket-timeout (socket-error) ()
  (:documentation "Socket connection timed out."))

(define-condition socket-refused (socket-error) ()
  (:documentation "Connection refused."))

(define-condition socket-aborted (socket-error) ()
  (:documentation "Connection aborted."))

(define-condition socket-address-in-use (socket-error) ()
  (:documentation "Address is already in use."))

;; TBD: socket-accept-error is not actually used currently
(define-condition socket-accept-error (socket-error)
  ((listener :accessor socket-accept-error-listener :initarg :listener :initform (cffi:null-pointer))
   (server :accessor socket-accept-error-server :initarg :server :initform nil))
  (:report (lambda (c s) (format s "Error accepting connection: ~a"
                                 (socket-accept-error-listener c))))
  (:documentation "Passed to a server's event-cb when there's an error accepting a connection."))

(define-condition-alias socket-closed streamish-closed)

(defclass socket (streamish)
  ((c :accessor socket-c)
   (data :accessor socket-data)
   (address :accessor socket-address)
   (closed :accessor socket-closed)
   (buffer :accessor socket-buffer :initarg :buffer :initform (make-buffer)
     :documentation "Holds data sent on the socket that hasn't been sent yet.")
   (bufferingp :accessor socket-buffering-p :initform nil
     :documentation "Lets us know if the socket is currently buffering output.")
   (connected :accessor socket-connected :initarg :connected :initform nil)
   (drain-read-buffer :accessor socket-drain-read-buffer))
  (:documentation "Wraps around a socket."))

(defmethod errno-event ((socket socket) (errno (eql (uv:errval :etimedout))))
  (make-instance 'socket-timeout :socket socket :code errno :msg "connection timed out"))

(defmethod errno-event ((socket socket) (errno (eql (uv:errval :econnreset))))
  (make-instance 'socket-reset :socket socket :code errno :msg "connection reset"))

(defmethod errno-event ((socket socket) (errno (eql (uv:errval :eof))))
  (make-instance 'socket-eof :socket socket))

(defmethod errno-event ((socket socket) (errno (eql (uv:errval :econnrefused))))
  (make-instance 'socket-refused :socket socket :code errno :msg "connection refused"))

(defmethod errno-event ((socket socket) (errno (eql (uv:errval :econnaborted))))
  (make-instance 'socket-aborted :socket socket :code errno :msg "connection aborted"))

;; applicable both to sockets and socket-servers
(defmethod errno-event ((socket t) (errno (eql (uv:errval :eaddrinuse))))
  (make-instance 'socket-address-in-use :socket socket :code errno
                                        :msg "address already in use"))

(defclass socket-server ()
  ((c :accessor socket-server-c :initarg :c :initform (cffi:null-pointer))
   (closed :accessor socket-server-closed :initarg :closed :initform nil)
   (stream :accessor socket-server-stream :initarg :stream :initform nil))
  (:documentation "Wraps around a connection listener."))

(defgeneric close-socket-server (socket)
  (:documentation
    "Closes a socket server. If already closed, does nothing."))

(defmethod close-socket-server ((socket-server socket-server))
  (unless (socket-server-closed socket-server)
    (setf (socket-server-closed socket-server) t)
    (let ((server-c (socket-server-c socket-server)))
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
         (socket (getf socket-data :streamish))
         (cur-read-timeout (getf (getf socket-data :read-timeout) :event))
         (cur-write-timeout (getf (getf socket-data :write-timeout) :event))
         (read-timeout (when read-sec
                         (delay (lambda ()
                                  (event-handler (uv:errval :etimedout)
                                                 ;; don't cache the event-cb, it may change
                                                 (getf (get-callbacks uvstream) :event-cb)
                                                 :streamish socket))
                                :time read-sec)))
         (write-timeout (when write-sec
                          (delay (lambda ()
                                   (event-handler (uv:errval :etimedout)
                                                  ;; don't cache the event-cb, it may change
                                                  (getf (get-callbacks uvstream) :event-cb)
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
         ;; is all flushed out in the socket-connect-cb.
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
                                  (buffer-output (socket-buffer socket)))
               (setf (socket-buffer socket) (make-buffer))))))
        (t
         (call-next-method))))

(defgeneric write-pending-socket-data (socket)
  (:documentation
    "Write any pending data on the given socket to its underlying stream."))

(defmethod write-pending-socket-data ((socket socket))
  (let ((pending (buffer-output (socket-buffer socket))))
    (setf (socket-buffer socket) (make-buffer))
    (write-socket-data socket pending :force t)))

(defmethod close-streamish ((socket socket) &key force &allow-other-keys)
  (when (and (not force) (socket-buffering-p socket))
    (write-pending-socket-data socket))
  (call-next-method))

(define-c-callback socket-connect-cb :void ((req :pointer) (status :int))
  "Called when an outgoing socket connects."
  (let* ((uvstream (deref-data-from-pointer req))
         (stream-data (deref-data-from-pointer uvstream))
         (socket (getf stream-data :streamish))
         (stream (getf stream-data :stream))
         (callbacks (get-callbacks uvstream))
         (event-cb (getf callbacks :event-cb))
         (connect-cb (getf callbacks :connect-cb)))
    (catch-app-errors event-cb
      (free-pointer-data req :preserve-pointer t)
      (uv:free-req req)
      (unless (zerop status)
        (unless (socket-closed-p socket)
          (run-event-cb 'event-handler status event-cb :streamish socket))
        (return-from socket-connect-cb))
      (setf (socket-connected socket) t)
      (when (and (not (socket-closed-p socket))
                 ;; start reading on the socket
                 (streamish-read-start socket))
        (when connect-cb
          (funcall connect-cb (or stream socket)))
        ;; write any buffered output we've stored up
        (write-pending-socket-data socket)))))

(defgeneric server-socket-class (server)
  (:documentation "Return socket class for connections accepted by SERVER"))

(defgeneric make-socket-handle (socket-or-server)
  (:documentation "Create an underlying stream handle for socket connection"))

(defmethod initialize-instance :after ((socket socket) &key &allow-other-keys)
  (setf (socket-c socket) (make-socket-handle socket)))

(defmethod initialize-instance :after ((server socket-server) &key &allow-other-keys)
  (setf (socket-server-c server) (make-socket-handle server)))

(defun init-incoming-socket (server status)
  "Called by the socket-accept-cb when an incoming connection is detected. Sets up
   a socket between the client and the server along with any callbacks the
   server has attached to it. Returns the cl-async socket object created."
  (let* ((server-instance (deref-data-from-pointer server))
         (callbacks (get-callbacks server))
         (read-cb (getf callbacks :read-cb))
         (event-cb (getf callbacks :event-cb))
         (connect-cb (getf callbacks :connect-cb)))
    (catch-app-errors event-cb
      (if (< status 0)
          ;; error! call the handler
          (run-event-cb 'event-handler status event-cb)
          ;; great, keep going
          (let* ((stream-data-p (socket-server-stream server-instance))
                 (socket (make-instance (server-socket-class server-instance)
                                        :direction :in
                                        :connected t
                                        :drain-read-buffer (not stream-data-p)))
                 (uvstream (socket-c socket))
                 (stream (when stream-data-p (make-instance 'async-io-stream :streamish socket))))
            (if (zerop (uv:uv-accept server uvstream))
                (progn
                  (attach-data-to-pointer uvstream (list :streamish socket :stream stream))
                  (save-callbacks uvstream (list :read-cb read-cb :event-cb event-cb))
                  (when connect-cb (funcall connect-cb (or stream socket)))
                  (uv:uv-read-start uvstream
                                    (cffi:callback streamish-alloc-cb)
                                    (cffi:callback streamish-read-cb)))
                (uv:uv-close uvstream (cffi:null-pointer))))))))

(define-c-callback socket-accept-cb :void ((server :pointer) (status :int))
  "Called by a listener when an incoming connection is detected. Thin wrapper
   around init-incoming-socket, which does all the setting up of callbacks and
   pointers and so forth."
  (init-incoming-socket server status))

(defun init-client-socket (socket-class
                           &key read-cb event-cb
                             data stream connect-cb write-cb
                             (direction :out)
                             (read-timeout -1)
                             (write-timeout -1)
                             (dont-drain-read-buffer nil dont-drain-read-buffer-supplied-p))
  "Initialize an async socket, but do not connect it."
  (check-event-loop-running)

  (let* ((dont-drain-read-buffer
           ;; assume dont-drain-read-buffer if unspecified and requesting a stream
           (if (and stream (not dont-drain-read-buffer-supplied-p))
               t
               dont-drain-read-buffer))
         (socket (make-instance socket-class
                                :direction direction
                                :drain-read-buffer (not dont-drain-read-buffer)))
         (uvstream (socket-c socket))
         (async-stream (when stream (make-instance 'async-io-stream :streamish socket))))
    (when data
      (write-socket-data socket data))
    (save-callbacks uvstream (list :read-cb read-cb
                                   :event-cb event-cb
                                   :write-cb write-cb
                                   :connect-cb connect-cb))
    ;; allow the socket/stream class to be referenced directly by the uvstream
    (attach-data-to-pointer uvstream (list :streamish socket
                                           :stream async-stream))
    ;; call this AFTER attach-data-to-pointer because this appends to the data
    (set-socket-timeouts uvstream read-timeout write-timeout :socket-is-uvstream t)
    (if stream
        async-stream
        socket)))

(defgeneric socket-server-bind (server address fd)
  (:documentation "Bind socket server to the specified address, return
  libuv errno. Specify FD to bind the server to an existing
  file descriptor."))

(defun socket-server (server-class address read-cb &key event-cb connect-cb backlog stream fd)
  "Start a socket listener on the current event loop. Returns a socket-server instance
   which can be closed with close-socket-server"
  (check-event-loop-running)
  (let* ((server-instance (make-instance server-class :stream stream))
         (server-c (socket-server-c server-instance))
         (r-bind (socket-server-bind server-instance address fd))
         (backlog (if (or (null backlog)
                          (< backlog 0))
                      128
                      backlog))
         (r-listen (when (zerop r-bind)
                     (uv:uv-listen server-c backlog (cffi:callback socket-accept-cb)))))
    ;; check that our listener instantiated properly
    (when (or (< r-bind 0)
              (< r-listen 0))
      (unwind-protect
           (event-handler (if (zerop r-bind) r-listen r-bind) event-cb :throw t :streamish server-instance)
        (close-socket-server server-instance))
      (return-from socket-server))
    ;; make sure the server is closed/freed on exit
    (add-event-loop-exit-callback (lambda ()
                                    (close-socket-server server-instance)))
    (attach-data-to-pointer server-c server-instance)
    ;; setup an accept error cb
    (save-callbacks server-c (list :read-cb read-cb :event-cb event-cb :connect-cb connect-cb))
    ;; return the listener, which can be closed by the app if needed
    server-instance))

;; note: there's no actual :async-socket handle type,
;; it's provided by handle-cleanup implementations for tcp and pipes
(defmethod handle-cleanup ((handle-type (eql :async-socket)) handle)
  (let* ((data (deref-data-from-pointer handle))
         (socket/server (if (listp data)
                            (getf data :streamish)
                            data)))
    (cond ((null data)
           ;; this may happen, for example, when tcp-connect
           ;; fails somewhere in the middle due to a bug
           (warn (format nil "an uv handle without corresponding object detected (~s)" (uv:handle-type handle)))
           (do-close-streamish handle :force t))
          ((typep socket/server 'socket-server)
           (unless (socket-server-closed socket/server)
             (close-socket-server socket/server)))
          ((not (socket-closed-p socket/server))
           (close-socket socket/server :force t)))))

;;;; compatibility

(defun write-socket-data (socket data &rest args &key &allow-other-keys)
  "An compatibility alias for STREAMISH-WRITE."
  (apply #'streamish-write socket data args))

(defun check-socket-open (socket)
  "Compatibility alias for CHECK-STREAMISH-OPEN."
  (check-streamish-open socket))

(defun socket-closed-p (socket)
  "Return whether a socket is closed or not.
  Same as streamish-closed-p."
  (streamish-closed-p socket))

(defun close-socket (socket &key force)
  "Free a socket (uvstream) and clear out all associated data.
  Same as close-streamish."
  (close-streamish socket :force force))

;; condition accessor
(defmethod socket ((thing t))
  (streamish thing))
