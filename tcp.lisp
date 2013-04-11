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
  ((c :accessor socket-c :initarg :c :initform (cffi:null-pointer) :type cffi:foreign-pointer)
   (data :accessor socket-data :initarg data :initform nil)
   (closed :accessor socket-closed :initarg :closed :initform nil :type boolean)
   (direction :accessor socket-direction :initarg :direction :initform nil :type symbol)
   (drain-read-buffer :accessor socket-drain-read-buffer :initarg :drain-read-buffer :initform t :type boolean))
  (:documentation "Wraps around a libevent bufferevent socket."))

(defclass tcp-server ()
  ((c :accessor tcp-server-c :initarg :c :initform (cffi:null-pointer) :type cffi:foreign-pointer)
   (closed :accessor tcp-server-closed :initarg :closed :initform nil :type boolean)
   (stream :accessor tcp-server-stream :initarg :stream :initform nil :type boolean)
   (data-pointer :accessor tcp-server-data-pointer :initarg :data-pointer :initform nil :type cffi:foreign-pointer))
  (:documentation "Wraps around a libevent connection listener."))

(defun* (get-last-tcp-err -> (values fixnum string)) ()
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

(declaim (inline check-socket-open))
(defun* (check-socket-open -> null) ((socket socket))
  "Throw a socket-closed condition if given a socket that's closed."
  (declare (optimize speed (debug 0)))
  (when (typep socket 'socket)
    (when (socket-closed socket)
      (error 'socket-closed :code -1 :msg "Trying to operate on a closed socket")))
  nil)

(defun* (socket-closed-p -> boolean) ((socket socket))
  "Return whether a socket is closed or not."
  (socket-closed socket))

(defgeneric close-socket (socket)
  (:documentation
    "Free a socket (bufferevent) and clear out all associated data."))

(defmethod close-socket ((socket socket))
  "Close and free a socket and all of it's underlying structures."
  ;; grab the data pointer associated with the bufferevent and free it. see
  ;; comment tcp-connect about data-pointer for a better explanation.
  (declare (optimize speed (debug 0)))
  (check-socket-open socket)
  (let* ((bev (socket-c socket))
         (bev-data (deref-data-from-pointer bev))
         (bev-data-pointer (getf bev-data :data-pointer)))
    (declare (type cffi:foreign-pointer bev bev-data-pointer))
    (le:bufferevent-disable bev (logior le:+ev-read+ le:+ev-write+))
    (if (eq (socket-direction socket) 'in)
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

(defun* set-socket-timeouts ((socket (or socket cffi:foreign-pointer)) (read-sec real) (write-sec real) &key ((socket-is-bufferevent boolean) nil))
  "Given a pointer to a libevent socket (bufferevent), set the read/write
   timeouts on the bufferevent."
  (declare (optimize speed (debug 0)))
  (when (subtypep (type-of socket) 'socket)
    (check-socket-open socket))
  (let ((socket (if socket-is-bufferevent
                    socket
                    (socket-c socket)))
        (read-sec (if (realp read-sec) read-sec -1))
        (write-sec (if (realp write-sec) write-sec -1)))
    ;; since it takes time/memory to instantiate foreign timeval objects, we only
    ;; do the bare minimum here (which means a lot of switching logic)
    (cond ((and (< read-sec 0)
                (< write-sec 0))
           (le:bufferevent-set-timeouts socket (cffi:null-pointer) (cffi:null-pointer)))
          ((and (< 0 read-sec)
                (< 0 write-sec))
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
                     (le:bufferevent-set-timeouts socket read-to write-to)))))))
          ((< 0 read-sec)
           (multiple-value-bind (read-sec read-usec) (split-usec-time read-sec)
             (make-foreign-type (read-to (le::cffi-type le::timeval))
                                (('le::tv-sec read-sec)
                                 ('le::tv-usec read-usec))
               (le:bufferevent-set-timeouts socket read-to (cffi:null-pointer)))))
          ((< 0 write-sec)
           (multiple-value-bind (write-sec write-usec) (split-usec-time write-sec)
             (make-foreign-type (write-to (le::cffi-type le::timeval))
                                (('le::tv-sec write-sec)
                                 ('le::tv-usec write-usec))
               (le:bufferevent-set-timeouts socket (cffi:null-pointer) write-to)))))
    nil))

(defun* (enabled-socket -> fixnum) ((socket socket) &key ((read boolean) nil) ((write boolean) nil))
  "Enable read/write monitoring on a socket. If :read or :write are nil, they
   are not disabled, but rather just not enabled."
  (declare (optimize speed (debug 0) (safety 0)))
  (check-socket-open socket)
  (let ((bev (socket-c socket)))
    (le:bufferevent-enable bev (logior (if read le:+ev-read+ 0)
                                       (if write le:+ev-write+ 0)))))

(defun* (disable-socket -> fixnum) ((socket socket) &key ((read boolean) nil) ((write boolean) nil))
  "Disable read/write monitoring on a socket. If :read or :write are nil, they
   are not enabled, but rather just not disabled."
  (declare (optimize speed (debug 0) (safety 0)))
  (check-socket-open socket)
  (let ((bev (socket-c socket)))
    (le:bufferevent-disable bev (logior (if read le:+ev-read+ 0)
                                        (if write le:+ev-write+ 0)))))

(defun* read-socket-data ((socket (or cffi:foreign-pointer socket)) (data-cb callback) &key ((socket-is-evbuffer boolean) nil))
  "Read the data out of a bufferevent (or directly, an evbuffer)."
  (declare (optimize speed (debug 0) (safety 0)))
  (when (subtypep (type-of socket) 'socket)
    (check-socket-open socket))
  (let ((bufsize *buffer-size*)
        (buffer-c *socket-buffer-c*)
        (buffer-lisp *socket-buffer-lisp*)
        (input (if socket-is-evbuffer
                   socket
                   (le:bufferevent-get-input (socket-c socket)))))
    (declare (type fixnum bufsize)
             (type cffi:foreign-pointer buffer-c input)
             (type octet-vector buffer-lisp))
    (loop for n = (the fixnum (le:evbuffer-remove input buffer-c bufsize))
          while (< 0 n) do
      (dotimes (i n)
        (declare (type fixnum i))
        (setf (aref buffer-lisp i) (cffi:mem-aref buffer-c :unsigned-char i)))
      (funcall data-cb (subseq buffer-lisp 0 n)))))

(defun* (read-bytes-from-socket -> octet-vector)
          ((socket (or cffi:foreign-pointer socket))
           (num-bytes fixnum)
          &key ((socket-is-evbuffer boolean) nil))
  "Read num-bytes out of a socket's buffer and return them. If the evbuffer has
   less bytes than num-bytes, returns nil (ie, try again later)."
  (declare (optimize speed (debug 0) (safety 0)))
  (let ((bufsize *buffer-size*)
        (buffer-c *socket-buffer-c*)
        (buffer-lisp *socket-buffer-lisp*)
        (input (if socket-is-evbuffer
                   socket
                   (le:bufferevent-get-input (socket-c socket))))
        (data-final nil))
    (declare (type fixnum bufsize)
             (type cffi:foreign-pointer buffer-c input)
             (type octet-vector buffer-lisp)
             (type (or null octet-vector) data-final))
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

(defun* write-to-evbuffer ((evbuffer cffi:foreign-pointer) (data bytes-or-string))
  "Writes data directly to evbuffer."
  (declare (optimize speed (debug 0) (safety 0)))
  (let* ((data (if (stringp data)
                   (babel:string-to-octets data :encoding :utf-8)
                   data))
         (data-length (length data))
         (data-index 0)
         (buffer-c *socket-buffer-c*))
    ;; copy into evbuffer using existing c buffer
    (loop while (< 0 data-length) do
      (let ((bufsize (min data-length *buffer-size*)))
        (declare (fixnum bufsize))
        (dotimes (i bufsize)
          (declare (fixnum i))
          (setf (cffi:mem-aref buffer-c :unsigned-char i) (aref data data-index))
          (incf data-index))
        (le:evbuffer-add evbuffer buffer-c bufsize)
        (decf data-length bufsize)))))

(defun* write-socket-data
          ((socket socket)
           (data bytes-or-string)
          &key ((read-cb callback) nil)
          ((write-cb callback) nil)
          ((event-cb callback) nil))
  "Write data into a cl-async socket. Allows specifying read/write/event
   callbacks. Any callback left nil will use that current callback from the
   socket (so they only override when specified, otherwise keep the current
   callback)"
  (declare (optimize speed (debug 0)))
  (check-socket-open socket)
  (let* ((bev (socket-c socket))
         ;; since this gets called in multiple places, wrap it in a function
         (do-send (lambda ()
                    ;; enable the bufferevent (callbacks and timeouts)
                    (le:bufferevent-enable bev (logior le:+ev-read+ le:+ev-write+))
                    ;; write our data to the sockets buffer
                    (write-to-evbuffer (le:bufferevent-get-output bev) data))))

    (declare (type cffi:foreign-pointer bev)
             (type function do-send))
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
              (declare (type cffi:foreign-pointer socket-data-pointer)
                       (type callback read-cb-current write-cb-current event-cb-current))
              (save-callbacks socket-data-pointer
                              (list :read-cb (if (functionp read-cb) read-cb read-cb-current)
                                    :write-cb (if (functionp write-cb) write-cb write-cb-current)
                                    :event-cb (if (functionp event-cb) event-cb event-cb-current))))
            (funcall do-send)))

        ;; we're not setting callbacks, so just enable the socket and send the
        ;; data
        (funcall do-send))
    nil))

(defun* (drain-evbuffer -> octet-vector) ((evbuffer cffi:foreign-pointer))
  "Grab all data in an evbuffer and put it into a byte array (returned)."
  (declare (optimize speed (debug 0)))
  (let ((body nil))
    (declare (type (or null octet-vector) body))
    (read-socket-data evbuffer
                      (lambda (data)
                        (setf body (if body
                                       (append-array body data)
                                       data)))
                      :socket-is-evbuffer t)
    body))

(cffi:defcallback tcp-read-cb :void ((bev :pointer) (data-pointer :pointer))
  "Called whenever a read event happens on a TCP socket. Ties into the anonymous
   callback system to run user-specified anonymous callbacks on read events."
  (declare (optimize speed (debug 0)))
  (let* ((bev-data (deref-data-from-pointer bev))
         (socket (getf bev-data :socket))
         (tcp-stream (getf bev-data :stream))
         (callbacks (get-callbacks data-pointer))
         (read-cb (getf callbacks :read-cb))
         (event-cb (getf callbacks :event-cb))
         (drain-read (socket-drain-read-buffer socket)))
    (declare (type socket socket)
             (type (or null async-stream) tcp-stream)
             (type callback read-cb event-cb)
             (type boolean drain-read))
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

(cffi:defcallback tcp-write-cb :void ((bev :pointer) (data-pointer :pointer))
  "Called when data is finished being written to a socket."
  (declare (optimize speed (debug 0)))
  (let* ((socket (getf (deref-data-from-pointer bev) :socket))
         (callbacks (get-callbacks data-pointer))
         (write-cb (getf callbacks :write-cb))
         (event-cb (getf callbacks :event-cb)))
    (declare (type socket socket)
             (type callback write-cb event-cb))
    (catch-app-errors event-cb
      (when write-cb
        (funcall write-cb socket)))))

(cffi:defcallback tcp-event-cb :void ((bev :pointer) (events :short) (data-pointer :pointer))
  "Called whenever anything happens on a TCP socket. Ties into the anonymous
   callback system to track failures/disconnects."
  (declare (optimize speed (debug 0)))
  (let* ((event nil)
         (dns-base (deref-data-from-pointer data-pointer))
         (bev-data (deref-data-from-pointer bev))
         (socket (getf bev-data :socket))
         (callbacks (get-callbacks data-pointer))
         (event-cb (getf callbacks :event-cb))
         (connect-cb (getf callbacks :connect-cb)))
    (declare (type (or null event-info) event)
             (type (or null cffi:foreign-pointer) dns-base)
             (type socket socket)
             (type callback event-cb connect-cb))
    (catch-app-errors event-cb
      (unwind-protect
        ;; if we just connected and we have a connect-cb, call it (only for
        ;; outgoing connections though, since incoming are handled in the
        ;; accept-cb)
        (when (and connect-cb
                   (eq (socket-direction socket) 'out)
                   (logand events le:+bev-event-connected+))
          (funcall connect-cb socket))
        ;; process any errors we received
        (cond
          ((< 0 (logand events (logior le:+bev-event-error+
                                       le:+bev-event-timeout+)))
           (multiple-value-bind (errcode errstr) (get-last-tcp-err)
             (let ((dns-err (le:bufferevent-socket-get-dns-error bev)))
               (cond
                 ;; DNS error
                 ((and (< 0 (logand events le:+bev-event-error+))
                       (not (zerop dns-err)))
                  (setf event (make-instance 'dns-error
                                             :code dns-err
                                             :msg (le:evutil-gai-strerror dns-err)))
                  (release-dns-base))

                 ;; socket timeout
                 ((< 0 (logand events le:+bev-event-timeout+))
                  (setf event (make-instance 'tcp-timeout :socket socket :code -1 :msg "Socket timed out")))

                 ;; connection reset by peer
                 ((or (eq errcode 104)
                      (< 0 (logand events le:+bev-event-eof+)))
                  (setf event (make-instance 'tcp-eof :socket socket)))

                 ;; since we don't know what the error was, just spawn a general
                 ;; error.
                 ((< 0 errcode)
                  (setf event (make-instance 'tcp-error :socket socket :code errcode :msg errstr)))
                 ;; libevent signaled an error, but nothing actually happened
                 ;; (that we know of anyway). ignore...
                 ;(t
                 ; (setf event (make-instance 'tcp-error :socket socket :code events :msg (format nil "Unkonwn error (~a): ~a" events errcode))))
                 ))))
          ;; peer closed connection.
          ((< 0 (logand events le:+bev-event-eof+))
           (setf event (make-instance 'tcp-eof :socket socket)))
          ((and dns-base
                (< 0 (logand events le:+bev-event-connected+))         
                (not (cffi:null-pointer-p dns-base)))
           (release-dns-base)))
        (when event
          (unwind-protect
            (when event-cb (run-event-cb event-cb event))
            ;; if the app closed the socket in the event cb (perfectly fine),
            ;; make sure we don't trigger an error trying to close it again.
            (handler-case (close-socket socket)
              (socket-closed () nil))))))))

(defun* (init-incoming-socket -> socket) ((bev cffi:foreign-pointer) (callbacks list) (server tcp-server))
  "Called by the tcp-accept-cb when an incoming connection is detected. Sets up
   a socket between the client and the server along with any callbacks the
   server has attached to it. Returns the cl-async socket object created."
  (declare (optimize speed (debug 0)))
  (let* ((per-conn-data-pointer (create-data-pointer))
         (stream-data-p (the boolean (tcp-server-stream server)))
         (socket (the socket (make-instance 'socket :c bev :direction 'in :drain-read-buffer (not stream-data-p))))
         (stream (when stream-data-p (make-instance 'async-io-stream :socket socket)))
         (event-cb (getf callbacks :event-cb))
         (connect-cb (getf callbacks :connect-cb)))
    (declare (type cffi:foreign-pointer per-conn-data-pointer)
             (type socket socket)
             (type (or null async-stream) stream)
             (type callback event-cb connect-cb))
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

(cffi:defcallback tcp-accept-cb :void ((listener :pointer) (fd :int) (addr :pointer) (socklen :int) (data-pointer :pointer))
  "Called by a listener when an incoming connection is detected. Thin wrapper
   around init-incoming-socket, which does all the setting up of callbacks and
   pointers and so forth."
  (declare (ignore socklen addr))
  (let* ((server (deref-data-from-pointer data-pointer))
         (callbacks (get-callbacks data-pointer))
         (event-base (le:evconnlistener-get-base listener))
         (bev (le:bufferevent-socket-new event-base fd +bev-opt-close-on-free+)))
    (declare (type tcp-server server)
             (type list callbacks)
             (type cffi:foreign-pointer bev))
    (init-incoming-socket bev callbacks server)))

(cffi:defcallback tcp-accept-err-cb :void ((listener :pointer) (data-pointer :pointer))
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

(defun* (init-tcp-socket -> (or socket async-stream))
          ((read-cb callback)
           (event-cb callback)
          &key ((data (or null bytes-or-string)) nil)
               ((stream boolean) nil)
               ((fd (or fixnum cffi:foreign-pointer)) -1)
               ((connect-cb callback) nil)
               ((write-cb callback) nil)
               ((read-timeout fixnum) -1)
               ((write-timeout fixnum) -1)
               ((dont-drain-read-buffer boolean) nil dont-drain-read-buffer-supplied-p))
  "Initialize an async socket, but do not connect it."
  (check-event-loop-running)

  (let* ((data-pointer (create-data-pointer))
         (fd (or fd -1))
         (bev (le:bufferevent-socket-new (event-base-c *event-base*) fd +bev-opt-close-on-free+))
         ;; assume dont-drain-read-buffer if unspecified and requesting a stream
         (dont-drain-read-buffer (if (and stream (not dont-drain-read-buffer-supplied-p))
                                     t
                                     dont-drain-read-buffer))
         (socket (make-instance 'socket :c bev
                                        :direction 'out
                                        :drain-read-buffer (not dont-drain-read-buffer)))
         (tcp-stream (when stream (make-instance 'async-io-stream :socket socket))))

    (declare (type cffi:foreign-pointer data-pointer bev)
             (type (or fixnum cffi:foreign-pointer) fd)
             (type boolean dont-drain-read-buffer)
             (type socket socket)
             (type (or null async-stream) tcp-stream))

    ;; be sure to mark the socket as nonblocking if we passed one in
    (unless (equal fd -1)
      (le:evutil-make-socket-nonblocking fd))

    (le:bufferevent-setcb bev
                          (cffi:callback tcp-read-cb)
                          (cffi:callback tcp-write-cb)
                          (cffi:callback tcp-event-cb)
                          data-pointer)
    (le:bufferevent-enable bev (logior le:+ev-read+ le:+ev-write+))
    (save-callbacks data-pointer (list :read-cb read-cb
                                       :event-cb event-cb
                                       :write-cb write-cb
                                       :connect-cb connect-cb))
    (when data
      (write-to-evbuffer (le:bufferevent-get-output bev) data))
    (set-socket-timeouts bev read-timeout write-timeout :socket-is-bufferevent t)

    ;; allow the data pointer/socket class to be referenced directly by the bev
    (attach-data-to-pointer bev (list :data-pointer data-pointer
                                      :socket socket
                                      :stream tcp-stream))
    (if stream
        tcp-stream
        socket)))

(defun* (connect-tcp-socket -> (or socket async-stream))
          ((socket/stream (or socket async-stream))
           (host (or string null))
           (port fixnum))
  "Connect a tcp socket initialized with init-tcp-socket."
  (let* ((socket (if (subtypep (type-of socket/stream) 'async-stream)
                     (stream-socket socket/stream)
                     socket/stream))
         (bev (socket-c socket))
         (data-pointer (getf (deref-data-from-pointer bev) :data-pointer)))
    (declare (type socket socket)
             (type cffi:foreign-pointer bev data-pointer))
    ;; track the connection
    (incf (event-base-num-connections-out *event-base*))
    ;; only connect if we didn't get an existing fd passed in
    (if (ip-address-p host)
        ;; got an IP so just connect directly
        (with-ip-to-sockaddr ((sockaddr sockaddr-size) host port)
          (le:bufferevent-socket-connect bev sockaddr sockaddr-size))

        ;; get a DNS base and do an async lookup
        (let ((dns-base (get-dns-base)))
          (attach-data-to-pointer data-pointer dns-base)
          (le:bufferevent-socket-connect-hostname bev dns-base *default-lookup-type* host port))))
  socket/stream)

(defun* (tcp-connect -> (or socket async-stream))
          ((host (or null string))
           (port fixnum)
           (read-cb callback)
           (event-cb callback)
          &key ((data (or null bytes-or-string)) nil)
               ((stream boolean) nil)
               ((connect-cb callback) nil)
               ((write-cb callback) nil)
               ((read-timeout fixnum) -1)
               ((write-timeout fixnum) -1)
               ((dont-drain-read-buffer boolean) nil dont-drain-read-buffer-supplied-p))
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
    (declare (type (or socket async-stream) socket/stream))
    (connect-tcp-socket socket/stream host port)
    socket/stream))

(defun* (tcp-server -> tcp-server)
          ((bind-address (or null string))
           (port fixnum)
           (read-cb callback)
           (event-cb callback)
          &key ((connect-cb callback) nil)
               ((backlog fixnum) -1)
               ((stream boolean) nil))
  "Start a TCP listener on the current event loop. Returns a tcp-server class
   which can be closed with close-tcp-server"
  (check-event-loop-running)
  (with-ip-to-sockaddr ((sockaddr sockaddr-size) bind-address port)
    (let* ((data-pointer (create-data-pointer))
           (listener (le:evconnlistener-new-bind (event-base-c *event-base*)
                                                  (cffi:callback tcp-accept-cb)
                                                  data-pointer
                                                  (logior le:+lev-opt-reuseable+ le:+lev-opt-close-on-free+)
                                                  backlog
                                                  sockaddr
                                                  sockaddr-size))
           (server-class (make-instance 'tcp-server
                                        :c listener
                                        :stream stream
                                        :data-pointer data-pointer)))
      ;; check that our listener instantiated properly
      (when (or (and (not (cffi:pointerp listener))
                     (zerop listener))
                (cffi:null-pointer-p listener))
        (free-pointer-data data-pointer)
        (error (make-instance 'tcp-server-bind-error :addr bind-address :port port)))
      ;; make sure the server is closed/freed on exit
      (add-event-loop-exit-callback (lambda ()
                                      (close-tcp-server server-class)
                                      (free-pointer-data data-pointer)))
      (attach-data-to-pointer data-pointer server-class)
      ;; setup an accept error cb
      (le:evconnlistener-set-error-cb listener (cffi:callback tcp-accept-err-cb))
      (save-callbacks data-pointer (list :read-cb read-cb :event-cb event-cb :connect-cb connect-cb))
      ;; return the listener, which can be closed by the app if needed
      server-class)))

