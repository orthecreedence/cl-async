(in-package :cl-async)

(defparameter *buffer-size* 16384
  "The amount of data we'll pull from the evbuffers when doing reading/writing.")

(defconstant +sockaddr-size+ (cffi:foreign-type-size (le::cffi-type le::sockaddr-in))
  "Really no sense in computing this OVER AND OVER.")

(define-condition connection-info ()
  ((connection :initarg :connection :reader conn-fd :initform nil))
  (:report (lambda (c s) (format s "Connection info: ~a~%" (conn-fd c))))
  (:documentation "Describes the base condition for any action on a connection."))

(define-condition connection-eof (connection-info) ()
  (:report (lambda (c s) (format s "Connection EOF: ~a~%" (conn-fd c))))
  (:documentation "Passed to a failure callback when a peer closes the connection."))

(define-condition connection-error (connection-info)
  ((code :initarg :code :reader conn-errcode :initform 0)
   (msg :initarg :msg :reader conn-errmsg :initform nil))
  (:report (lambda (c s) (format s "Connection error: ~a: ~a~%" (conn-errcode c) (conn-errmsg c))))
  (:documentation "Describes a general connection error."))

(define-condition connection-timeout (connection-error) ()
  (:report (lambda (c s) (format s "Connection timeout: ~a: ~a~%" (conn-errcode c) (conn-errmsg c))))
  (:documentation "Passed to a failure callback when a connection times out."))

(define-condition connection-refused (connection-error) ()
  (:report (lambda (c s) (format s "Connection refused: ~a: ~a~%" (conn-errcode c) (conn-errmsg c))))
  (:documentation "Passed to a failure callback when a connection is refused."))

(define-condition connection-dns-error (connection-error) ()
  (:report (lambda (c s) (format s "Connection DNS error: ~a~%" (conn-df c))))
  (:documentation "Passed to a failure callback when a DNS error occurs on a connection."))

(defun get-last-tcp-err ()
  "Since libevent provides a macro but not a function for getting the last error
   code, we have to essentially rebuild that macro here.

   NOTE: As libevent says, this function cannot be considered idempotent. In
   other words, calling it more than once may result in different values
   returned. So, call once and save the result until the next time an error
   happens."
  #+(or :windows :win32 :win64)
    (let ((code (cffi:foreign-funcall "WSAGetLastError" :int)))
      (values code
              (cffi:foreign-funcall "evutil_socket_error_to_string" :int code :string)))
  #-(or :windows :win32 :win64)
    (let ((code (cffi:mem-aref (cffi:foreign-symbol-pointer "errno") :int)))
      (values code
              (cffi:foreign-funcall "strerror" :int code :string))))

(defun close-socket (bev)
  "Free a socket and clear out all associated data."
  (clear-callbacks bev)
  (le:evutil-closesocket (le:bufferevent-getfd bev))
  (le:bufferevent-free bev))

(defun free-listener (listener)
  "Free a socket listener and all associated data."
  (clear-callbacks listener)
  (le:evconnlistener-free listener))

(defun set-socket-timeouts (socket read-sec write-sec)
  "Given a pointer to a libevent socket (bufferevent), set the read/write
   timeouts on the bufferevent."
  (multiple-value-bind (read-sec read-usec) (split-usec-time read-sec)
    (multiple-value-bind (write-sec write-usec) (split-usec-time write-sec)
      (make-foreign-type (read-to (le::cffi-type le::timeval))
                         (('le::tv-sec read-sec)
                          ('le::tv-usec read-usec))
        (make-foreign-type (write-to (le::cffi-type le::timeval))
                           (('le::tv-sec write-sec)
                            ('le::tv-usec write-usec))
          (let ((read-to (if (numberp read-sec) read-to (cffi:null-pointer)))
                (write-to (if (numberp write-sec) write-to (cffi:null-pointer))))
          (le:bufferevent-set-timeouts socket read-to write-to)))))))

(defun read-socket-data (socket data-cb &key socket-is-evbuffer)
  "Read the data out of a bufferevent (or directly, an evbuffer)."
  (let ((bufsize *buffer-size*))
    (cffi:with-foreign-object (buffer :unsigned-char bufsize)
      (let ((input (if socket-is-evbuffer
                       socket
                       (le:bufferevent-get-input socket)))
            (lbuf (make-array bufsize :element-type '(unsigned-byte 8))))
        (loop for n = (le:evbuffer-remove input buffer bufsize)
              while (< 0 n) do
          (dotimes (i n)
            (setf (aref lbuf i) (cffi:mem-aref buffer :char i)))
          (funcall data-cb (subseq lbuf 0 n)))))))

(defun write-socket-data (socket data &key socket-is-evbuffer)
  "Write data into libevent socket (bufferevent)."
  (let ((data (if (stringp data)
                  (babel:string-to-octets data :encoding :utf-8)
                  data))
        (evbuffer (if socket-is-evbuffer
                      socket
                      (le:bufferevent-get-output socket))))
    (cffi:with-foreign-object (data-c :unsigned-char (length data))
      (dotimes (i (length data))
        (setf (cffi:mem-aref data-c :unsigned-char i) (aref data i)))
      (le:evbuffer-add evbuffer data-c (length data)))))

(defun drain-evbuffer (evbuffer)
  "Grab all data in an evbuffer and put it into a byte array (returned)."
  (let ((body (make-array 0 :element-type '(unsigned-byte 8))))
    (read-socket-data evbuffer
                      (lambda (data)
                        (setf body (append-array body data :element-type '(unsigned-byte 8))))
                      :socket-is-evbuffer t)
    body))

(cffi:defcallback tcp-read-cb :void ((bev :pointer) (event-base :pointer))
  "Called whenever a read event happens on a TCP socket. Ties into the anonymous
   callback system to run user-specified anonymous callbacks on read events."
  (declare (ignore event-base))
  (let ((read-cb (getf (get-callbacks bev) :read-cb)))
    (if read-cb
        ;; we have a callback, so grab the data form the socket and send it in
        (read-socket-data bev (lambda (data) (funcall read-cb bev data)))
        ;; no callback associated, so just drain the buffer so it doesn't pile up
        (le:evbuffer-drain (le:bufferevent-get-input bev) *buffer-size*))))
  
(cffi:defcallback tcp-event-cb :void ((bev :pointer) (events :short) (event-base :pointer))
  "Called whenever anything happens on a TCP socket. Ties into the anonymous
   callback system to track failures/disconnects."
  (declare (ignore event-base))
  (cond
    ((< 0 (logand events le:+bev-event-connected+))
     nil)
    ((< 0 (logand events (logior le:+bev-event-error+
                                 le:+bev-event-eof+
                                 le:+bev-event-timeout+)))
     (unwind-protect
       (multiple-value-bind (errcode errstr) (get-last-tcp-err)
         (let ((err nil)
               (connection (le:bufferevent-getfd bev))
               (dns-err (le:bufferevent-socket-get-dns-error bev))
               (fail-cb (getf (get-callbacks bev) :fail-cb)))
           (cond
             ;; DNS error
             ((and (< 0 (logand events le:+bev-event-error+))
                   (not (zerop dns-err)))
              (setf err (make-instance 'connection-dns-error
                                       :connection connection
                                       :msg (le:evutil-gai-strerror dns-err))))

             ;; socket timeout
             ((< 0 (logand events le:+bev-event-timeout+))
              (setf err (make-instance 'connection-timeout :connection connection :code errcode :msg errstr)))

             ;; peer closed connection.
             ((< 0 (logand events le:+bev-event-eof+))
              (setf err (make-instance 'connection-eof :connection connection :code errcode :msg errstr)))

             ;; since we don't know what the error was, just spawn a general
             ;; error.
             (t
              (setf err (make-instance 'connection-error :connection connection :code errcode :msg errstr))))
           (funcall fail-cb err)))
       (close-socket bev)))))

(cffi:defcallback tcp-accept-cb :void ((listener :pointer) (fd :int) (addr :pointer) (socklen :int) (ctx :pointer))
  "Called when a connection is accepted. Creates a bufferevent for the socket
   and sets up the callbacks onto that socket."
  (declare (ignore ctx socklen addr))
  (let* ((event-base (le:evconnlistener-get-base listener))
         (bev (le:bufferevent-socket-new event-base
                                          fd
                                          (cffi:foreign-enum-value 'le:bufferevent-options :+bev-opt-close-on-free+)))
         (callbacks (get-callbacks listener)))
    ;; make sure the socket is non-blocking
    (let ((nonblock (le:evutil-make-socket-nonblocking (le:bufferevent-getfd bev))))
      (unless (zerop nonblock)
        (error "Failed to make socket non-blocking: ~a~%" nonblock)))

    ;; save the callbacks given to the listener onto each socket individually
    (save-callbacks bev callbacks)
    (le:bufferevent-setcb bev
                           (cffi:callback tcp-read-cb)
                           (cffi:null-pointer)
                           (cffi:callback tcp-event-cb)
                           (cffi:null-pointer))
    ;(set-socket-timeouts bev 5 5)
    (le:bufferevent-enable bev (logior le:+ev-read+ le:+ev-write+))))

(cffi:defcallback tcp-accept-err-cb :void ((listener :pointer) (ctx :pointer))
  "Called when an error occurs accepting a connection."
  (declare (ignore ctx))
  (let* ((event-base (le:evconnlistener-get-base listener)))
    (format t "There was an error and I don't know how to get the code.~%")
    (le:event-base-loopexit event-base (cffi:null-pointer))))

(defparameter *ip-scanner*
  (cl-ppcre:create-scanner
    "^[0-9]{1,3}(\\.[0-9]{1,3}){3}$"
    :case-insensitive-mode t)
  "Scanner that detects if a string is an IP.")

(defun ip-address-p (host)
  "Determine if the given host is an IP or a hostname."
  (cl-ppcre:scan *ip-scanner* host))

(defun tcp-send (host port data read-cb fail-cb &key ((:socket bev)) (read-timeout 30) (write-timeout 30))
  "Open a TCP connection asynchronously. An event loop must be running for this
   to work."
  (check-event-loop-running)
  (let* ((bev-exists-p bev)
         (bev (if bev
                  bev
                  (le:bufferevent-socket-new *event-base* -1 (cffi:foreign-enum-value 'le:bufferevent-options :+bev-opt-close-on-free+)))))
    (le:bufferevent-setcb bev (cffi:callback tcp-read-cb) (cffi:null-pointer) (cffi:callback tcp-event-cb) *event-base*)
    (le:bufferevent-enable bev (logior le:+ev-read+ le:+ev-write+))
    (save-callbacks bev (list :read-cb read-cb :fail-cb fail-cb))
    (write-socket-data bev data)
    (set-socket-timeouts bev read-timeout write-timeout)

    (unless bev-exists-p
      ;; connect the socket
      (if (ip-address-p host)
          ;; spawn a DNS base and do an async lookup
          (let ((dns-base (le:evdns-base-new *event-base* 1)))
            (le:bufferevent-socket-connect-hostname bev dns-base le:+af-unspec+ host port))

          ;; got an IP so just connect directly
          (make-foreign-type (sockaddr (le::cffi-type le::sockaddr-in) :initial #x0)
                             (('le::sin-family le:+af-inet+)
                              ('le::sin-port (cffi:foreign-funcall "htons" :int port :unsigned-short))
                              ('le::sin-addr (cffi:foreign-funcall "inet_addr" :string host :unsigned-long)))
            (le:bufferevent-socket-connect bev
                                            sockaddr
                                            +sockaddr-size+))))))

(defun tcp-server (bind-address port read-cb fail-cb)
  "Start a TCP listener on the current event loop."
  (check-event-loop-running)
  (make-foreign-type (sockaddr (le::cffi-type le::sockaddr-in) :initial #x0)
                     (('le::sin-family le:+af-inet+)
                      ('le::sin-port (cffi:foreign-funcall "htons" :int port :unsigned-short))
                      ('le::sin-addr (if bind-address
                                         (cffi:foreign-funcall "inet_addr" :string bind-address :unsigned-long)
                                         (cffi:foreign-funcall "htonl" :unsigned-long 0 :unsigned-long))))
    (let* ((listener (le:evconnlistener-new-bind *event-base*
                                                  (cffi:callback tcp-accept-cb)
                                                  (cffi:null-pointer)
                                                  (logior le:+lev-opt-reuseable+ le:+lev-opt-close-on-free+)
                                                  -1
                                                  sockaddr
                                                  +sockaddr-size+)))
      (add-event-loop-exit-callback (lambda () (free-listener listener)))
      (when (and (not (cffi:pointerp listener)) (zerop listener))
        (error "Couldn't create listener: ~a~%" listener))
      ;(le:evconnlistener-set-error-cb listener (cffi:callback tcp-accept-err-cb))
      (save-callbacks listener (list :read-cb read-cb :fail-cb fail-cb)))))

