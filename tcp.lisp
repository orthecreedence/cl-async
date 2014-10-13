(in-package :cl-async)

(define-condition tcp-info (event-info)
  ((socket :initarg :socket :accessor tcp-socket :initform nil))
  (:documentation "Base TCP condition. Holds the socket object."))

(define-condition tcp-error (event-error tcp-info) ()
  (:report (lambda (c s) (format s "TCP connection error: ~a: ~a" (event-errcode c) (event-errmsg c))))
  (:documentation "Describes a general TCP connection error."))

(define-condition tcp-eof (tcp-info) ()
  (:report (lambda (c s) (format s "TCP connection EOF: ~a" (tcp-socket c))))
  (:documentation "Passed to an event callback when a peer closes a TCP connection."))

(define-condition tcp-reset (tcp-error) ()
  (:report (lambda (c s) (format s "TCP connection reset ~a: ~a" (event-errcode c) (event-errmsg c))))
  (:documentation "Passed to an event callback when a TCP connection times out."))

(define-condition tcp-timeout (tcp-error) ()
  (:report (lambda (c s) (format s "TCP connection timeout: ~a: ~a" (event-errcode c) (event-errmsg c))))
  (:documentation "Passed to an event callback when a TCP connection times out."))

(define-condition tcp-refused (tcp-error) ()
  (:report (lambda (c s) (format s "TCP connection refused: ~a: ~a" (event-errcode c) (event-errmsg c))))
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
   (data :accessor socket-data :initarg data :initform nil)
   (closed :accessor socket-closed :initarg :closed :initform nil)
   (direction :accessor socket-direction :initarg :direction :initform nil)
   (drain-read-buffer :accessor socket-drain-read-buffer :initarg :drain-read-buffer :initform t))
  (:documentation "Wraps around a libevent bufferevent socket."))

(defclass tcp-server ()
  ((c :accessor tcp-server-c :initarg :c :initform (cffi:null-pointer))
   (closed :accessor tcp-server-closed :initarg :closed :initform nil)
   (stream :accessor tcp-server-stream :initarg :stream :initform nil)
   (data-pointer :accessor tcp-server-data-pointer :initarg :data-pointer :initform nil))
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
    (when (socket-closed socket)
      (error 'socket-closed :code -1 :msg "Trying to operate on a closed socket" :socket socket))))

(defun socket-closed-p (socket)
  "Return whether a socket is closed or not."
  (socket-closed socket))

(defgeneric close-socket (socket)
  (:documentation
    "Free a socket (bufferevent) and clear out all associated data."))

(defmethod close-socket ((socket socket))
  "Close and free a socket and all of it's underlying structures."
  ;; grab the data pointer associated with the bufferevent and free it. see
  ;; comment tcp-connect about data-pointer for a better explanation.
  (check-socket-open socket)
  (let* ((bev (socket-c socket))
         (bev-data (deref-data-from-pointer bev))
         (bev-data-pointer (getf bev-data :data-pointer)))
    (le:bufferevent-disable bev (logior le:+ev-read+ le:+ev-write+))
    (if (eq (socket-direction socket) :in)
        (decf (event-base-num-connections-in *event-base*))
        (decf (event-base-num-connections-out *event-base*)))
    (le:bufferevent-free bev)
    (setf (socket-closed socket) t)
    (free-pointer-data bev-data-pointer)
    (free-pointer-data bev :preserve-pointer t)))

(defgeneric close-tcp-server (socket)
  (:documentation
    "Closes a TCP server. If already closed, does nothing."))

(defmethod close-tcp-server ((tcp-server tcp-server))
  (unless (tcp-server-closed tcp-server)
    (le:evconnlistener-free (tcp-server-c tcp-server))
    (setf (tcp-server-closed tcp-server) t)))

(defun set-socket-timeouts (socket read-sec write-sec &key socket-is-bufferevent)
  "Given a pointer to a libevent socket (bufferevent), set the read/write
   timeouts on the bufferevent."
  (check-socket-open socket)
  (let ((socket (if socket-is-bufferevent
                    socket
                    (socket-c socket)))
        (read-sec (if (numberp read-sec) read-sec -1))
        (write-sec (if (numberp write-sec) write-sec -1)))
    (multiple-value-bind (read-sec read-usec) (split-usec-time read-sec)
      (multiple-value-bind (write-sec write-usec) (split-usec-time write-sec)
        (make-foreign-type (read-to (le::cffi-type le::timeval))
                           (('le::tv-sec read-sec)
                            ('le::tv-usec read-usec))
          (make-foreign-type (write-to (le::cffi-type le::timeval))
                             (('le::tv-sec write-sec)
                              ('le::tv-usec write-usec))
            (let ((read-to (if (< 0 read-sec) read-to (cffi:null-pointer)))
                  (write-to (if (< 0 write-sec) write-to (cffi:null-pointer))))
              (le:bufferevent-set-timeouts socket read-to write-to))))))))

(defun enable-socket (socket &key read write)
  "Enable read/write monitoring on a socket. If :read or :write are nil, they
   are not disabled, but rather just not enabled."
  (check-socket-open socket)
  (let ((bev (socket-c socket)))
    (le:bufferevent-enable bev (logior (if read le:+ev-read+ 0)
                                       (if write le:+ev-write+ 0)))))

(defun disable-socket (socket &key read write)
  "Disable read/write monitoring on a socket. If :read or :write are nil, they
   are not enabled, but rather just not disabled."
  (check-socket-open socket)
  (let ((bev (socket-c socket)))
    (le:bufferevent-disable bev (logior (if read le:+ev-read+ 0)
                                        (if write le:+ev-write+ 0)))))

(defun read-socket-data (socket data-cb &key socket-is-evbuffer)
  "Read the data out of a bufferevent (or directly, an evbuffer)."
  (check-socket-open socket)
  (let ((bufsize *buffer-size*)
        (buffer-c *socket-buffer-c*)
        (buffer-lisp *socket-buffer-lisp*)
        (input (if socket-is-evbuffer
                   socket
                   (le:bufferevent-get-input (socket-c socket)))))
    (loop for n = (le:evbuffer-remove input buffer-c bufsize)
          while (< 0 n) do
      (dotimes (i n)
        (setf (aref buffer-lisp i) (cffi:mem-aref buffer-c :unsigned-char i)))
      (funcall data-cb (subseq buffer-lisp 0 n)))))

(defun read-bytes-from-socket (socket num-bytes &key socket-is-evbuffer)
  "Read num-bytes out of a socket's buffer and return them. If the evbuffer has
   less bytes than num-bytes, returns nil (ie, try again later)."
  (let ((bufsize *buffer-size*)
        (buffer-c *socket-buffer-c*)
        (buffer-lisp *socket-buffer-lisp*)
        (input (if socket-is-evbuffer
                   socket
                   (le:bufferevent-get-input (socket-c socket))))
        (data-final nil))
    (loop for bytes-to-read = (min num-bytes bufsize (le:evbuffer-get-length input))
          for n = (le:evbuffer-remove input buffer-c bytes-to-read)
          while (< 0 n) do
      (dotimes (i n)
        (setf (aref buffer-lisp i) (cffi:mem-aref buffer-c :unsigned-char i)))
      (let ((read-bytes (subseq buffer-lisp 0 n)))
        (setf data-final (if data-final
                             (append-array data-final read-bytes)
                             read-bytes)))
      (decf num-bytes n))
    data-final))

(defun write-to-evbuffer (evbuffer data)
  "Writes data directly to evbuffer."
  (let* ((data (if (stringp data)
                   (babel:string-to-octets data :encoding :utf-8)
                   data))
         (data-length (length data))
         (data-index 0)
         (buffer-c *socket-buffer-c*))
    ;; copy into evbuffer using existing c buffer
    (loop while (< 0 data-length) do
      (let ((bufsize (min data-length *buffer-size*)))
        (dotimes (i bufsize)
          (setf (cffi:mem-aref buffer-c :unsigned-char i) (aref data data-index))
          (incf data-index))
        (le:evbuffer-add evbuffer buffer-c bufsize)
        (decf data-length bufsize)))))

(defun write-socket-data (socket data &key read-cb write-cb event-cb)
  "Write data into a cl-async socket. Allows specifying read/write/event
   callbacks. Any callback left nil will use that current callback from the
   socket (so they only override when specified, otherwise keep the current
   callback)"
  (check-socket-open socket)

  (let* ((bev (socket-c socket))
         ;; since this gets called in multiple places, wrap it in a function
         (do-send (lambda ()
                    ;; enable the bufferevent (callbacks and timeouts)
                    (le:bufferevent-enable bev (logior le:+ev-read+ le:+ev-write+))
                    ;; write our data to the sockets buffer
                    (write-to-evbuffer (le:bufferevent-get-output bev) data))))

    (if (or read-cb write-cb event-cb)
        ;; we're specifying callbacks. since we're most likely calling this from
        ;; inside a socket callback and we don't necessarily want to overwrite
        ;; that socket's callbacks until it finishes, we set a delay here so the
        ;; callback binding happens after the caller returns to the event loop.
        (as:delay
          (lambda ()
            (let* ((bev-data (deref-data-from-pointer bev))
                   (socket-data-pointer (getf bev-data :data-pointer))
                   (callbacks (get-callbacks socket-data-pointer))
                   (read-cb-current (getf callbacks :read-cb))
                   (write-cb-current (getf callbacks :write-cb))
                   (event-cb-current (getf callbacks :event-cb)))
              (save-callbacks socket-data-pointer
                              (list :read-cb (if (functionp read-cb) read-cb read-cb-current)
                                    :write-cb (if (functionp write-cb) write-cb write-cb-current)
                                    :event-cb (if (functionp event-cb) event-cb event-cb-current))))
            (funcall do-send)))

        ;; we're not setting callbacks, so just enable the socket and send the
        ;; data
        (funcall do-send))))

(defun drain-evbuffer (evbuffer)
  "Grab all data in an evbuffer and put it into a byte array (returned)."
  (let ((body nil))
    (read-socket-data evbuffer
                      (lambda (data)
                        (setf body (if body
                                       (append-array body data)
                                       data)))
                      :socket-is-evbuffer t)
    body))

(define-c-callback tcp-alloc-cb :void ((handle :pointer) (size :unsigned-int))
  (declare (ignore handle))
  (uv:uv-buf-init (cffi:foreign-alloc :char :count size) size))

(defun tcp-event-cb (socket errno event-cb)
  "Called when an event (error, mainly) occurs on a TCP stream."
  (let* ((event nil)
         (errstr "TODO: grab error strings from libuv"))
    (catch-app-errors event-cb
      (unwind-protect
        (cond
          ((= errno uv:+uv--etimedout+)
           (setf event (make-instance 'tcp-timeout :socket socket :code errno)))
          ((= errno uv:+uv--econnreset+)
           (setf event (make-instance 'tcp-reset :socket socket :code errno)))
          ((= errno uv:+uv--econnrefused+)
           (setf event (make-instance 'tcp-refused :socket socket :code errno)))
          ((= errno uv:+uv--eof+)
           (setf event (make-instance 'tcp-eof :socket socket :code errno)))
          (t
           (setf event (make-instance 'tcp-error :socket socket :code errno :msg errstr))))
        (when event
          (unwind-protect
            (when event-cb (run-event-cb event-cb event))
            ;; if the app closed the socket in the event cb (perfectly fine),
            ;; make sure we don't trigger an error trying to close it again.
            (handler-case (close-socket socket)
              (socket-closed () nil))))))))

(define-c-callback tcp-read-cb :void ((uv-stream :pointer) (nread :int) (buf :pointer))
  (let* ((stream-data (deref-data-from-pointer uv-stream))
         (socket (getf stream-data :socket))
         (stream (getf stream-data :stream))
         (data-pointer (getf stream-data :data-pointer))
         (callbacks (get-callbacks data-pointer))
         (read-cb (getf callbacks :read-cb))
         (event-cb (getf callbacks :event-cb))
         (buf-ptr (uv-a:uv-buf-t-base buf))
         (buf-len (uv-a:uv-buf-t-len buf)))
    (catch-app-errors event-cb
      (when (< nread 0)
        ;; we got an error
        (unless (cffi:null-pointer-p buf-ptr)
          (cffi:foreign-free buf-ptr))
        (uv:uv-close uv-stream (cffi:null-pointer))
        (tcp-event-cb socket nread event-cb)
        (return-from tcp-read-cb))

      ;; read the buffer
      (let ((bytes (make-array buf-len :element-type '(unsigned-byte 8))))
        (dotimes (i buf-len)
          (setf (aref bytes i) (cffi:mem-aref buf-ptr :unsigned-char i)))
        (funcall read-cb socket bytes))
      
      ;; free the buffer
      (unless (cffi:null-pointer-p buf-ptr)
        (cffi:foreign-free buf-ptr)))))

(define-c-callback tcp-connect-cb :void ((uv-stream :pointer) (status :int))
  (let* ((stream-data (deref-data-from-pointer uv-stream))
         (socket (getf stream-data :socket))
         (stream (getf stream-data :stream))
         (data-pointer (getf stream-data :data-pointer))
         (callbacks (get-callbacks data-pointer))
         (connect-cb (getf callbacks :connect-cb)))
    (when connect-cb
      (funcall connect-cb (or stream socket)))
    (uv:uv-read-start uv-stream (cffi:callback tcp-alloc-cb) (cffi:callback tcp-read-cb))))

(define-c-callback tcp-read-cb :void ((bev :pointer) (data-pointer :pointer))
  "Called whenever a read event happens on a TCP socket. Ties into the anonymous
   callback system to run user-specified anonymous callbacks on read events."
  (let* ((bev-data (deref-data-from-pointer bev))
         (socket (getf bev-data :socket))
         (tcp-stream (getf bev-data :stream))
         (callbacks (get-callbacks data-pointer))
         (read-cb (getf callbacks :read-cb))
         (event-cb (getf callbacks :event-cb))
         (drain-read (socket-drain-read-buffer socket)))
    (catch-app-errors event-cb
      (cond ((and read-cb drain-read)
             ;; we have a drain callback, so grab the data form the socket and
             ;; send it in
             (read-socket-data socket (lambda (data) (funcall read-cb socket data))))
            (read-cb
             ;; we got a non-draining callback, let the read-cb know that we
             ;; have data without emptying the read buffer
             (funcall read-cb socket tcp-stream))
            (drain-read
             ;; no callback associated, and we're asking to drain, to clean out
             ;; the evbuffer without calling a cb
             (le:evbuffer-drain (le:bufferevent-get-input bev) *buffer-size*))
            (t
             ;; we're not draining and we don't have a callback, so this is
             ;; probably some sort of stream operation
             nil)))))

(define-c-callback tcp-write-cb :void ((bev :pointer) (data-pointer :pointer))
  "Called when data is finished being written to a socket."
  (let* ((socket (getf (deref-data-from-pointer bev) :socket))
         (callbacks (get-callbacks data-pointer))
         (write-cb (getf callbacks :write-cb))
         (event-cb (getf callbacks :event-cb)))
    (catch-app-errors event-cb
      (when write-cb
        (funcall write-cb socket)))))

(defun init-incoming-socket (bev callbacks server)
  "Called by the tcp-accept-cb when an incoming connection is detected. Sets up
   a socket between the client and the server along with any callbacks the
   server has attached to it. Returns the cl-async socket object created."
  (let* ((per-conn-data-pointer (create-data-pointer))
         (stream-data-p (tcp-server-stream server))
         (socket (make-instance 'socket :c bev :direction :in :drain-read-buffer (not stream-data-p)))
         (stream (when stream-data-p (make-instance 'async-io-stream :socket socket)))
         (event-cb (getf callbacks :event-cb))
         (connect-cb (getf callbacks :connect-cb)))
    (catch-app-errors event-cb
      ;; attach our data-pointer/socket/stream class to the bev
      (attach-data-to-pointer bev (list :data-pointer per-conn-data-pointer
                                        :socket socket
                                        :stream stream))

      ;; make sure socket is non-blocking
      (le:evutil-make-socket-nonblocking (le:bufferevent-getfd bev))
      
      ;; track the connection. will be decf'ed when close-socket is called
      (incf (event-base-num-connections-in *event-base*))

      ;; if we have a connect-cb, call it
      (when connect-cb
        (funcall connect-cb socket))

      ;; save the callbacks given to the listener onto each socket individually
      (save-callbacks per-conn-data-pointer callbacks)
      (le:bufferevent-setcb bev
                            (cffi:callback tcp-read-cb)
                            (cffi:callback tcp-write-cb)
                            (cffi:callback tcp-event-cb)
                            per-conn-data-pointer)
      (set-socket-timeouts bev -1 -1 :socket-is-bufferevent t)
      (le:bufferevent-enable bev (logior le:+ev-read+ le:+ev-write+))
      socket)))

(define-c-callback tcp-accept-cb :void ((listener :pointer) (fd :int) (addr :pointer) (socklen :int) (data-pointer :pointer))
  "Called by a listener when an incoming connection is detected. Thin wrapper
   around init-incoming-socket, which does all the setting up of callbacks and
   pointers and so forth."
  (declare (ignore socklen addr))
  (let* ((server (deref-data-from-pointer data-pointer))
         (callbacks (get-callbacks data-pointer))
         (event-base (le:evconnlistener-get-base listener))
         (bev (le:bufferevent-socket-new event-base fd +bev-opt-close-on-free+)))
    (init-incoming-socket bev callbacks server)))

(define-c-callback tcp-accept-err-cb :void ((listener :pointer) (data-pointer :pointer))
  "Called when an error occurs accepting a connection."
  (let* ((tcp-server (deref-data-from-pointer data-pointer))
         (callbacks (get-callbacks data-pointer))
         (event-cb (getf callbacks :event-cb)))
    (catch-app-errors event-cb
      (run-event-cb event-cb (make-instance 'tcp-accept-error
                                            :code -1
                                            :msg "Error accepting client connection"
                                            :listener listener
                                            :tcp-server tcp-server)))))

(defun init-tcp-socket (read-cb event-cb &key data stream connect-cb write-cb (read-timeout -1) (write-timeout -1) (dont-drain-read-buffer nil dont-drain-read-buffer-supplied-p))
  "Initialize an async socket, but do not connect it."
  (check-event-loop-running)

  (let* ((data-pointer (create-data-pointer))
         (uvstream (cffi:foreign-alloc '(:pointer (:struct uv:uv-tcp-s))))
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
      (let* ((old-connect-cb connect-cb)
             (new-connect-cb (lambda (sock)
                               (write-socket-data sock data)
                               (funcall old-connect-cb sock))))
        (setf connect-cb new-connect-cb)))
    (save-callbacks data-pointer (list :read-cb read-cb
                                       :event-cb event-cb
                                       :write-cb write-cb
                                       :connect-cb connect-cb))
    (set-socket-timeouts uvstream read-timeout write-timeout)

    ;; allow the data pointer/socket class to be referenced directly by the uvstream
    (attach-data-to-pointer uvstream (list :data-pointer data-pointer
                                           :socket socket
                                           :stream tcp-stream))
    (if stream
        tcp-stream
        socket)))

(defun connect-tcp-socket (socket/stream host port &key event-cb)
  "Connect a tcp socket initialized with init-tcp-socket."
  (let* ((socket (if (subtypep (type-of socket/stream) 'async-stream)
                     (stream-socket socket/stream)
                     socket/stream))
         ;; since libevent doesn't use Windows' /etc/hosts, we have to basically
         ;; go behind its back here.
         (host (if (and (hash-table-p *windows-local-hosts*)
                        (gethash host *windows-local-hosts*))
                   (gethash host *windows-local-hosts*)
                   host))
         (uvstream (socket-c socket)))
    ;; track the connection
    (incf (event-base-num-connections-out *event-base*))
    ;; only connect if we didn't get an existing fd passed in
    (flet ((do-connect (ip port)
             (with-ip-to-sockaddr ((sockaddr) ip port)
               (cffi:with-foreign-object (req '(:pointer (:struct uv:uv-connect-s)))
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
            event-cb))))
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

(defun tcp-server (bind-address port read-cb event-cb &key connect-cb (backlog -1) stream)
  "Start a TCP listener on the current event loop. Returns a tcp-server class
   which can be closed with close-tcp-server"
  (check-event-loop-running)
  (cffi:with-foreign-object (server '(:pointer (:struct uv:uv-tcp-s)))
    (with-ip-to-sockaddr ((sockaddr sockaddr-size) bind-address port)
      (uv:uv-tcp-init (event-base-c *event-base*) server)
      (uv:uv-tcp-bind server sockaddr 0)
      (let* ((data-pointer (create-data-pointer))
             (server-class (make-instance 'tcp-server
                                          :c server
                                          :stream stream
                                          :data-pointer data-pointer))
             (r (uv:uv-listen server 128 (cffi:callback tcp-accept-cb))))
        ;; check that our listener instantiated properly
        (when (zerop r)
          (free-pointer-data data-pointer)
          (error (make-instance 'tcp-server-bind-error :addr bind-address :port port)))
        ;; make sure the server is closed/freed on exit
        (add-event-loop-exit-callback (lambda ()
                                        (close-tcp-server server-class)
                                        (free-pointer-data data-pointer)))
        (attach-data-to-pointer data-pointer server-class)
        ;; setup an accept error cb
        (save-callbacks data-pointer (list :read-cb read-cb :event-cb event-cb :connect-cb connect-cb))
        ;; return the listener, which can be closed by the app if needed
        server-class)))

