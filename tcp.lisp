(in-package :cl-async)

(defconstant +sockaddr-size+ (cffi:foreign-type-size (le::cffi-type le::sockaddr-in))
  "Really no sense in computing this OVER AND OVER.")

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

(defun close-socket (socket)
  "Free a socket (bufferevent) and clear out all associated data."
  (le:bufferevent-disable socket (logior le:+ev-read+ le:+ev-write+))
  ;; grab the data pointer associated with the bufferevent and free it. see
  ;; comment tcp-send about data-pointer for a better explanation.
  (let ((bev-data-pointer (deref-data-from-pointer socket))
        (fd (le::bufferevent-getfd socket)))
    (le:bufferevent-free socket)
    (le:evutil-closesocket fd)
    (free-pointer-data bev-data-pointer)
    (free-pointer-data socket :preserve-pointer t)))

(defun free-listener (listener)
  "Free a socket listener and all associated data."
  (free-pointer-data listener :preserve-pointer t)
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
  (let ((bufsize *buffer-size*)
        (buffer-c *socket-buffer-c*)
        (buffer-lisp *socket-buffer-lisp*)
        (input (if socket-is-evbuffer
                   socket
                   (le:bufferevent-get-input socket))))
    (loop for n = (le:evbuffer-remove input buffer-c bufsize)
          while (< 0 n) do
      (dotimes (i n)
        (setf (aref buffer-lisp i) (cffi:mem-aref buffer-c :char i)))
      (funcall data-cb (subseq buffer-lisp 0 n)))))

(defun write-socket-data (socket data &key socket-is-evbuffer)
  "Write data into libevent socket."
  (let* ((data (if (stringp data)
                   (babel:string-to-octets data :encoding :utf-8)
                   data))
         (data-length (length data))
         (data-index 0)
         (evbuffer (if socket-is-evbuffer
                       socket
                       (le:bufferevent-get-output socket)))
         (buffer-c *socket-buffer-c*))
    ;; copy into evbuffer using existing c buffer
    (loop while (< 0 data-length) do
      (let ((bufsize (min data-length *buffer-size*)))
      (dotimes (i bufsize)
        (setf (cffi:mem-aref buffer-c :unsigned-char i) (aref data data-index))
        (incf data-index))
      (le:evbuffer-add evbuffer buffer-c bufsize)
      (decf data-length bufsize)))))

(defun drain-evbuffer (evbuffer)
  "Grab all data in an evbuffer and put it into a byte array (returned)."
  (let ((body nil))
    (read-socket-data evbuffer
                      (lambda (data)
                        (if body
                            (setf body (append-array body data :element-type '(unsigned-byte 8)))
                            data))
                      :socket-is-evbuffer t)
    body))

(cffi:defcallback tcp-read-cb :void ((bev :pointer) (data-pointer :pointer))
  "Called whenever a read event happens on a TCP socket. Ties into the anonymous
   callback system to run user-specified anonymous callbacks on read events."
  (let* ((callbacks (get-callbacks data-pointer))
         (read-cb (getf callbacks :read-cb))
         (event-cb (getf callbacks :event-cb)))
    (catch-app-errors event-cb
      (if read-cb
          ;; we have a callback, so grab the data form the socket and send it in
          (read-socket-data bev (lambda (data) (funcall read-cb bev data)))
          ;; no callback associated, so just drain the buffer so it doesn't pile up
          (le:evbuffer-drain (le:bufferevent-get-input bev) *buffer-size*)))))

(cffi:defcallback tcp-event-cb :void ((bev :pointer) (events :short) (data-pointer :pointer))
  "Called whenever anything happens on a TCP socket. Ties into the anonymous
   callback system to track failures/disconnects."
  (let ((err nil)
        (connection (le:bufferevent-getfd bev))
        (dns-base (deref-data-from-pointer data-pointer))
        (event-cb (getf (get-callbacks data-pointer) :event-cb)))
    (catch-app-errors event-cb
      (unwind-protect
        (cond
          ((< 0 (logand events (logior le:+bev-event-error+
                                       le:+bev-event-timeout+)))
           (multiple-value-bind (errcode errstr) (get-last-tcp-err)
             (let ((dns-err (le:bufferevent-socket-get-dns-error bev)))
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

                 ;; since we don't know what the error was, just spawn a general
                 ;; error.
                 (t
                  (setf err (make-instance 'connection-error :connection connection :code errcode :msg errstr)))))))
          ;; peer closed connection.
          ((< 0 (logand events le:+bev-event-eof+))
           (setf err (make-instance 'connection-eof :connection connection)))
          ((< 0 (logand events le:+bev-event-connected+))
           (free-dns-base dns-base)))
        (when err
          (unwind-protect
            (when event-cb (funcall event-cb err))
            (close-socket bev)))))))

(cffi:defcallback tcp-accept-cb :void ((listener :pointer) (fd :int) (addr :pointer) (socklen :int) (data-pointer :pointer))
  "Called when a connection is accepted. Creates a bufferevent for the socket
   and sets up the callbacks onto that socket."
  (declare (ignore ctx socklen addr))
  (let* ((per-conn-data-pointer (create-data-pointer))
         (event-base (le:evconnlistener-get-base listener))
         (bev (le:bufferevent-socket-new event-base
                                         fd
                                         (cffi:foreign-enum-value 'le:bufferevent-options :+bev-opt-close-on-free+)))
         (callbacks (get-callbacks data-pointer))
         (event-cb (getf callbacks :event-cb)))
    (catch-app-errors event-cb
      ;; attach the per-conn-data-pointer to the bev
      (attach-data-to-pointer bev per-conn-data-pointer)

      ;; make sure the socket is non-blocking
      (let ((nonblock (le:evutil-make-socket-nonblocking (le:bufferevent-getfd bev))))
        (unless (zerop nonblock)
          (error "Failed to make socket non-blocking: ~a~%" nonblock)))

      ;; save the callbacks given to the listener onto each socket individually
      (save-callbacks per-conn-data-pointer callbacks)
      (le:bufferevent-setcb bev
                            (cffi:callback tcp-read-cb)
                            (cffi:null-pointer)
                            (cffi:callback tcp-event-cb)
                            per-conn-data-pointer)
      ;(set-socket-timeouts bev 5 5)
      (le:bufferevent-enable bev (logior le:+ev-read+ le:+ev-write+)))))

(cffi:defcallback tcp-accept-err-cb :void ((listener :pointer) (ctx :pointer))
  "Called when an error occurs accepting a connection."
  (declare (ignore ctx))
  (let* ((event-base (le:evconnlistener-get-base listener)))
    ;; TODO: why does the error handler segfault?!?!?
    (format t "There was an error and I don't know how to get the code.~%")
    (le:event-base-loopexit event-base (cffi:null-pointer))))

(defun tcp-send (host port data read-cb event-cb &key ((:socket bev)) (read-timeout 30) (write-timeout 30))
  "Open a TCP connection asynchronously. An event loop must be running for this
   to work."
  (check-event-loop-running)

  ;; this may get nasty...create a reference where data-pointer points to the
  ;; callbacks and dns-base for the current bufferevent, and the current
  ;; bufferevent has a reference to the current data-pointer. this is so that
  ;; when a bufferevent is re-used with new callbacks, the original data-pointer
  ;; isn't lost (since this would essentially be a memory leak, and cause a lot
  ;; of callbacks/dns-bases to be un-garbage-collectable...disaster). this way
  ;; is messy, but ensures that the bufferevent has only one data-pointer
  ;; attached to it for the duration of its life.
  (let* ((data-pointer (deref-data-from-pointer bev))
         (data-pointer-exists-p data-pointer)
         (data-pointer (if data-pointer-exists-p
                           data-pointer
                           (create-data-pointer)))
         (bev-exists-p bev)
         (bev (if bev-exists-p
                  bev
                  (le:bufferevent-socket-new *event-base* -1 (cffi:foreign-enum-value 'le:bufferevent-options :+bev-opt-close-on-free+)))))
    (le:bufferevent-setcb bev (cffi:callback tcp-read-cb) (cffi:null-pointer) (cffi:callback tcp-event-cb) data-pointer)
    (le:bufferevent-enable bev (logior le:+ev-read+ le:+ev-write+))
    (unless data-pointer-exists-p
      ;; make sure the data pointer is attached to the bufferevent so we don't
      ;; lose track of it if the bufferevent is reused.
      (attach-data-to-pointer bev data-pointer))
    (save-callbacks data-pointer (list :read-cb read-cb :event-cb event-cb))
    (write-socket-data bev data)
    (set-socket-timeouts bev read-timeout write-timeout)

    (unless bev-exists-p
      ;; connect the socket
      (if (ip-address-p host)
          ;; got an IP so just connect directly
          (with-ipv4-to-sockaddr (sockaddr host port)
            (le:bufferevent-socket-connect bev sockaddr +sockaddr-size+))

          ;; spawn a DNS base and do an async lookup
          (let ((dns-base (le:evdns-base-new *event-base* 1)))
            (attach-data-to-pointer data-pointer dns-base)
            (le:bufferevent-socket-connect-hostname bev dns-base le:+af-unspec+ host port))))))

(defun tcp-server (bind-address port read-cb event-cb)
  "Start a TCP listener on the current event loop."
  (check-event-loop-running)
  (with-ipv4-to-sockaddr (sockaddr bind-address port)
    (let* ((data-pointer (create-data-pointer))
           (listener (le:evconnlistener-new-bind *event-base*
                                                  (cffi:callback tcp-accept-cb)
                                                  data-pointer
                                                  (logior le:+lev-opt-reuseable+ le:+lev-opt-close-on-free+)
                                                  -1
                                                  sockaddr
                                                  +sockaddr-size+)))
      (add-event-loop-exit-callback (lambda ()
                                      (free-listener listener)
                                      (free-pointer-data data-pointer)))
      (when (and (not (cffi:pointerp listener)) (zerop listener))
        (error "Couldn't create listener: ~a~%" listener))
      ;(le:evconnlistener-set-error-cb listener (cffi:callback tcp-accept-err-cb))
      (save-callbacks data-pointer (list :read-cb read-cb :event-cb event-cb)))))

