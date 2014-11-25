(in-package :cl-async-ssl)

(define-condition tcp-ssl-error (tcp-error) ()
  (:documentation "Describes a general SSL connection error."))

(defclass as-ssl ()
  ((ctx :accessor as-ssl-ctx :initarg :ctx :initform nil)
   (bio-read :accessor as-ssl-bio-read :initarg :bio-read :initform nil)
   (bio-write :accessor as-ssl-bio-write :initarg :bio-write :initform nil)
   (ssl :accessor as-ssl-ssl :initarg :ssl :initform nil)
   (freed :accessor as-ssl-freed :initform nil))
  (:documentation "Wraps SSL connection information in a nice package"))

(defclass ssl-socket (socket)
  ((as-ssl :accessor socket-as-ssl :initarg :as-ssl :initform nil)
   (ssl-closing :accessor socket-ssl-closing :initform nil))
  (:documentation "Extends cl-async's socket to hold SSL info"))

(defclass tcp-ssl-server (tcp-server)
  ((as-ssl :accessor tcp-server-ssl-as-ssl :initarg :ssl-ctx :initform nil))
  (:documentation "Wraps around a libevent SSL servre/listener"))

(defun close-ssl (as-ssl)
  "Close up a cl-async SSL object."
  (when (as-ssl-freed as-ssl)
    (return-from close-ssl))
  (let* ((ctx (as-ssl-ctx as-ssl))
         (ssl (as-ssl-ssl as-ssl)))
    (when (cffi:pointerp ctx)
      (ssl-ctx-free ctx))
    (when (cffi:pointerp ssl)
      (free-pointer-data ssl :preserve-pointer t)
      (ssl-free ssl))
    (setf (as-ssl-freed as-ssl) t)))
  
(defmethod close-socket ((socket ssl-socket) &key force)
  (declare (ignore force))
  (let* ((as-ssl (socket-as-ssl socket))
         (res (ssl-shutdown (as-ssl-ssl as-ssl))))
    (call-next-method)
    (close-ssl as-ssl)))

(defmethod close-tcp-server ((tcp-server tcp-ssl-server))
  ;; shut down the listener before freeing the SSL handles
  (call-next-method)
  ;; free the SSL ctx (if it exists)
  (close-ssl (tcp-server-ssl-as-ssl tcp-server)))

(defun write-to-ssl (as-ssl data)
  "Write data into SSL."
  (let ((ssl (as-ssl-ssl as-ssl)))
    (do-chunk-data data *output-buffer*
      (lambda (buffer bufsize)
        (ssl-write ssl (static-vectors:static-vector-pointer buffer) bufsize)))))

(defmethod write-socket-data ((socket ssl-socket) data &key read-cb write-cb event-cb start end force write-ssl)
  (declare (ignore force start end))
  (format t "- write: (encrypted? ~a) ~a~%" (not write-ssl) (length (subseq data (or start 0) end)))
  (unless write-ssl
    (return-from write-socket-data (call-next-method)))
  (let ((uvstream (socket-c socket))
        (do-send (lambda ()
                   (write-to-ssl (socket-as-ssl socket) data))))
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

(defun ssl-run-state (ssl)
  (let* ((data (deref-data-from-pointer ssl))
         (socket (getf data :socket))
         (stream (getf data :stream))
         (drain-read (socket-drain-read-buffer socket))
         (callbacks (get-callbacks (as:socket-c socket)))
         ;(connect-cb (getf callbacks :connect-cb))
         (read-cb (getf callbacks :read-cb))
         ;(write-cb (getf callbacks :write-cb))
         ;(event-cb (getf callbacks :event-cb))
         (as-ssl (socket-as-ssl socket))
         (bio-write (as-ssl-bio-write as-ssl)))
    (loop for nread = (ssl-bio-read bio-write (static-vectors:static-vector-pointer *output-buffer*) (length *output-buffer*))
          while (< 0 nread) do
      (write-socket-data socket *output-buffer* :end nread))
    (let ((ret (ssl-read ssl (static-vectors:static-vector-pointer *input-buffer*) (length *input-buffer*))))
      (cond ((<= ret 0)
             (let* ((err (ssl-get-error ssl ret))
                    (str (cond
                           ((= err +ssl-error-ssl+)
                            (ssl-err-error-string (ssl-err-get-error) (cffi:null-pointer)))
                           ((= err +ssl-error-want-read+)
                             "want-read")
                           ((= err +ssl-error-want-write+)
                            "want-write")
                           ((= err +ssl-error-want-accept+)
                            "want-accept")
                           ((= err +ssl-error-want-connect+)
                            "want-connect"))))
               (format t "- err(~a): ~a~%" err str)))
            ((and read-cb drain-read)
             (funcall read-cb socket (subseq *input-buffer* 0 ret)))
            (stream
             (stream-append-bytes stream (subseq *input-buffer* 0 ret))
             (when read-cb (funcall read-cb socket stream)))))))

(define-c-callback tcp-ssl-info-cb :void ((ssl :pointer) (where :int) (ret :int))
  "Called whenever *stuff* happens on an SSL object."
  (let ((w (logand where +ssl-st-mask+))
        (state (ssl-state-string-long ssl)))
    (format t "- ssl: info: (where ~a/~a) (fin ~a) ~a~%" where ret (ssl-is-init-finished ssl) state)
    (cond ((& w +ssl-st-connect+) )
          ((& w +ssl-cb-handshake-done+) )
          ((& w +ssl-cb-handshake-start+) )
          ((& w +ssl-st-accept+) )
          ((& w +ssl-cb-read+) )
          ((& w +ssl-cb-write+) )
          ((& w +ssl-cb-alert+)
           (let ((read-alert (& w +ssl-cb-read+))
                 (err (ssl-alert-type-string-long ret))
                 (desc (ssl-alert-desc-string-long ret)))
             (format t "- ssl: alert: (~s): ~a / ~a~%" (if read-alert :read :write) err desc)))
          ((& w +ssl-cb-exit+) )
          ((& w +ssl-cb-loop+)
           (ssl-run-state ssl)))))

(defun tcp-ssl-connect (host port read-cb event-cb &key data stream connect-cb write-cb (read-timeout -1) (write-timeout -1) (dont-drain-read-buffer nil dont-drain-read-buffer-supplied-p))
  "Create and return an SSL-activated socket."
  (check-event-loop-running)
  (labels ((connect-cb (sock)
             (ssl-run-state (as-ssl-ssl (socket-as-ssl sock)))
             (when connect-cb
               (funcall connect-cb sock)))
           (read-cb (sock data)
             (format t "- read: (encrypted? ~a) ~a~%" t (length data))
             (let* ((as-ssl (socket-as-ssl sock))
                    (ssl (as-ssl-ssl as-ssl))
                    (bio-read (as-ssl-bio-read as-ssl)))
               (do-chunk-data data *input-buffer*
                 (lambda (buffer bufsize)
                   (ssl-bio-write bio-read (static-vectors:static-vector-pointer buffer) bufsize)))
               (ssl-run-state ssl))))
    (let* ((socket/stream (apply #'as:tcp-connect
                                 (append (list host port
                                               read-cb event-cb
                                               :data data
                                               :stream stream
                                               :connect-cb connect-cb
                                               :write-cb write-cb
                                               :read-timeout read-timeout
                                               :write-timeout write-timeout
                                               :class 'ssl-socket)
                                         (when dont-drain-read-buffer-supplied-p
                                           (list :dont-drain-read-buffer dont-drain-read-buffer)))))
           (socket (if (typep socket/stream 'as:async-stream)
                       (as:stream-socket socket/stream)
                       socket/stream))
           (stream (when (typep socket/stream 'as:async-stream) socket/stream))
           (socket-c (as:socket-c socket))
           (callbacks (get-callbacks socket-c))
           (method (ssl-tls-v1-client-method))
           (ctx (ssl-ctx-new method)))
      (ssl-ctx-set-default-verify-paths ctx)
      (ssl-ctx-set-verify ctx +ssl-verify-none+ (cffi:null-pointer))
      (setf (getf callbacks :connect-cb) #'connect-cb)
      (setf (getf callbacks :read-cb) #'read-cb)
      (save-callbacks socket-c callbacks)
      (let ((ssl (ssl-new ctx))
            (bio-read (ssl-bio-new (ssl-bio-s-mem)))
            (bio-write (ssl-bio-new (ssl-bio-s-mem))))
        (ssl-set-cipher-list ssl "ALL:!ADH:!LOW:!EXP:!MD5:@STRENGTH")
        (ssl-set-bio ssl bio-read bio-write)
        (ssl-set-info-callback ssl (cffi:callback tcp-ssl-info-cb))
        (ssl-bio-set-mem-eof-return bio-read -1)
        (ssl-bio-set-mem-eof-return bio-write -1)
        ;; TODO: cleanup: remove pointer on close
        (attach-data-to-pointer ssl (list :socket socket
                                          :stream stream))
        (setf (socket-as-ssl socket) (make-instance 'as-ssl
                                                    :ctx ctx
                                                    :ssl ssl
                                                    :bio-read bio-read
                                                    :bio-write bio-write))
        (ssl-connect ssl))
      socket/stream)))

#|
(define-c-callback ssl-event-cb :void ((bev :pointer) (events :short) (data-pointer :pointer))
  "Called whenever anything happens on a TCP socket. Ties into the anonymous
   callback system to track failures/disconnects."
  (let* ((bev-data (deref-data-from-pointer bev))
         (socket (getf bev-data :socket))
         (event-cb (getf (get-callbacks data-pointer) :event-cb)))
    (catch-app-errors event-cb
      ;; look specifically for SSL errors
      (let ((errcode (le-ssl:bufferevent-get-openssl-error bev))
            (ssl-error nil))
        (unless (zerop errcode)
          ;; we found an error. grab the errstring and call the event-cb
          (cffi:with-foreign-object (buf :unsigned-char 256)
            (cl+ssl::err-error-string errcode buf)
            (let ((str (cffi:foreign-string-to-lisp buf)))
              (unless (search "lib(0):func(1):reason" str)
                (setf ssl-error t)
                ;; make sure to close the socket after an error
                (close-socket socket)
                (when event-cb
                  (let ((str (if (= errcode 336351298)
                                 (format nil "~a (This can happen if a client uses a global context in the same process as a server. Try `:ssl-ctx (cl+ssl::ssl-ctx-new (cl+ssl::ssl-v23-client-method))` in yout tcp-ssl-connect call." str)
                                 str)))
                    (run-event-cb event-cb (make-instance 'tcp-ssl-error :code errcode :msg str))))))))
        ;; call directly into the tcp-event-cb function (if we haven't already
        ;; dealt with the error)
        (unless ssl-error
          (cffi:foreign-funcall-pointer
            (cffi:callback tcp-event-cb) ()
            :pointer bev :short events :pointer data-pointer))))))

(define-c-callback tcp-ssl-accept-cb :void ((listener :pointer) (fd :int) (addr :pointer) (socklen :int) (data-pointer :pointer))
  "Called when a connection is accepted on an SSL listener. Inits a socket with
   the client via init-incoming-socket and then wraps that socket in SSL."
  (declare (ignore socklen addr))
  (let* ((pointer-data (deref-data-from-pointer data-pointer))
         (callbacks (get-callbacks data-pointer))
         (server (getf pointer-data :server))
         (server-ctx (getf pointer-data :ctx))
         (event-base (le:evconnlistener-get-base listener))
         (client-ctx (cl+ssl::ssl-new server-ctx))
         (bev (le-ssl:bufferevent-openssl-socket-new
                event-base fd client-ctx 
                (cffi:foreign-enum-value 'le-ssl:bufferevent-ssl-state ':bufferevent-ssl-accepting)
                +bev-opt-close-on-free+)))
    (init-incoming-socket bev callbacks server)))

(defun init-ssl-client-context (global-ctx)
  "Initialize an SSL client context."
  (let ((client-ctx (cl+ssl::ssl-new global-ctx)))
    (cl+ssl::ssl-ctx-ctrl client-ctx
                          cl+ssl::+SSL_CTRL_MODE+ 
                          cl+ssl::+SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER+
                          (cffi:null-pointer))
    (cl+ssl::ssl-set-connect-state client-ctx)
    (when (zerop (cl+ssl::ssl-set-cipher-list client-ctx "ALL"))
      (error 'cl+ssl::ssl-error-initialize :reason "Can't set SSL cipher list"))
    ;; make sure we init PROPERLY
    (when (cffi:null-pointer-p client-ctx)
      (error "Problem initializing SSL context."))
    client-ctx))
    
(defun init-tcp-ssl-socket (read-cb event-cb &key data stream ssl-ctx (fd -1) connect-cb write-cb (read-timeout -1) (write-timeout -1) (dont-drain-read-buffer nil dont-drain-read-buffer-supplied-p))
  "Initialize an async SSL socket, but do not connect it."
  (check-event-loop-running)

  ;; make sure SSL is ready to go
  (cl+ssl:ensure-initialized :method 'cl+ssl::ssl-v23-client-method)
  (let* ((data-pointer (create-data-pointer))
         (ssl-ctx (or ssl-ctx cl+ssl::*ssl-global-context*))
         (client-ctx (init-ssl-client-context ssl-ctx))
         (fd (or fd -1))
         (bev (le-ssl:bufferevent-openssl-socket-new (event-base-c *event-base*)
                                                     fd
                                                     client-ctx
                                                     (cffi:foreign-enum-value 'le-ssl:bufferevent-ssl-state ':bufferevent-ssl-connecting)
                                                     (logior +bev-opt-close-on-free+
                                                             +bev-opt-defer-callbacks+)))

         ;; assume dont-drain-read-buffer if unspecified and requesting a stream
         (dont-drain-read-buffer (if (and stream (not dont-drain-read-buffer-supplied-p))
                                     t
                                     dont-drain-read-buffer))
         (socket (make-instance 'ssl-socket :c bev
                                            :direction :out
                                            :ctx client-ctx
                                            :drain-read-buffer (not dont-drain-read-buffer)))
         (tcp-stream (when stream (make-instance 'async-io-stream :socket socket))))

    ;; error check
    (when (cffi:null-pointer-p bev)
      (cl+ssl::ssl-free client-ctx)
      (error (format nil "Error creating SSL filter around socket: ~a~%" socket)))

    ;; be sure to mark the socket as nonblocking if we passed one in
    (unless (equal fd -1)
      (le:evutil-make-socket-nonblocking fd))

    (le:bufferevent-setcb bev
                          (cffi:callback tcp-read-cb)
                          (cffi:callback tcp-write-cb)
                          (cffi:callback ssl-event-cb)
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

(defun tcp-ssl-connect (host port read-cb event-cb
                        &key data stream ssl-ctx
                             connect-cb write-cb
                             (read-timeout -1) (write-timeout -1)
                             (dont-drain-read-buffer nil dont-drain-read-buffer-supplied-p))
  "Open a TCP connection asynchronously. Optionally send data out once connected
   via the :data keyword (can be a string or byte array)."
  (let* ((socket/stream (apply #'init-tcp-ssl-socket
                               (append (list read-cb event-cb
                                             :ssl-ctx ssl-ctx 
                                             :data data
                                             :stream stream
                                             :connect-cb connect-cb
                                             :write-cb write-cb
                                             :read-timeout read-timeout
                                             :write-timeout write-timeout)
                                       (when dont-drain-read-buffer-supplied-p
                                         (list :dont-drain-read-buffer dont-drain-read-buffer))))))
    ;; connect async, since this solves a problem where the socket starts to
    ;; write *before* SSL inits (bad news bears)
    (as:delay (lambda () (connect-tcp-socket socket/stream host port))
              :time 0
              :event-cb event-cb)
    socket/stream))

;; TODO: Figure out why read/write timeouts are borked when wrapping an existing
;; bufferevent. Until then, this function should be treated with caution (ie,
;; not exposed in the API)
(defun wrap-in-ssl (socket/stream
                    &key (ssl-context cl+ssl::*ssl-global-context*))
  "Wraps a cl-async socket/stream in the SSL protocol using libevent's SSL
   capabilities. Does some trickery when swapping out the event function for the
   new socket, but the original event-cb will still be called."
  ;; make sure SSL is ready to go
  (cl+ssl:ensure-initialized :method 'cl+ssl::ssl-v23-client-method)

  (let* ((global-ctx ssl-context)
         (passed-in-stream-p (subtypep (type-of socket/stream) 'async-stream))
         ;; whether we got a socket or a stream, grab a socket
         (socket (if passed-in-stream-p
                     (stream-socket socket/stream)
                     socket/stream))
         (bufferevent-orig (socket-c socket))
         ;; create an SSL oontext
         (ssl-ctx (init-ssl-client-context global-ctx)))
    ;; create our SSL socket/stream and make sure it grabs any callbacks/hooks
    ;; it needs to operate properly from the original bufferevent that
    ;; tcp-connect created.
    (let* ((state (cffi:foreign-enum-value 'le-ssl:bufferevent-ssl-state ':bufferevent-ssl-connecting))
           (ssl-bev (le-ssl:bufferevent-openssl-filter-new
                      (event-base-c *event-base*)
                      bufferevent-orig  ; pass in the original bev
                      ssl-ctx
                      state
                      ;; yes, close on free
                      (cffi:foreign-enum-value 'le:bufferevent-options ':+bev-opt-close-on-free+)))
           ;; make an SSL socket that matches the options of the original socket
           (ssl-socket (make-instance 'ssl-socket
                                      :ctx ssl-ctx
                                      :c ssl-bev
                                      :underlying socket
                                      :drain-read-buffer (socket-drain-read-buffer socket)))
           (ssl-tcp-stream (when passed-in-stream-p
                             (make-instance 'async-io-stream :socket ssl-socket))))
      ;; error check
      (when (cffi:null-pointer-p ssl-bev)
        (cl+ssl::ssl-free ssl-ctx)
        (error (format nil "Error creating SSL filter around socket: ~a~%" socket)))

      ;; reattach data/callbacks from the old bev/socket to the new bev/socket.
      ;; also, free our original data pointers 
      (let ((data-pointer (getf (deref-data-from-pointer bufferevent-orig) :data-pointer)))
        ;; we can just assign the original data pointer from the underlying
        ;; socket to the new socket and our callbacks/data magically move over
        (attach-data-to-pointer ssl-bev (list :data-pointer data-pointer
                                              :socket ssl-socket
                                              :stream ssl-tcp-stream))

        ;; unassign any data on the original bev
        (free-pointer-data bufferevent-orig :preserve-pointer t)

        ;; setup the callbacks for our ssl bufferevent. notice we hijack the event
        ;; callback (since we need some special processing). it will call the
        ;; normal tcp-event-cb when it's done processing.
        (le:bufferevent-setcb ssl-bev
                              (cffi:callback tcp-read-cb)
                              (cffi:callback tcp-write-cb)
                              (cffi:callback ssl-event-cb)
                              data-pointer))

      ;; NOTE: disabling socket timeouts for now
      ;; match up the socket timeouts
      (set-socket-timeouts socket nil nil)
      (set-socket-timeouts ssl-socket nil nil)
      
      ;; make sure the new socket is enabled
      (le:bufferevent-enable ssl-bev (logior le:+ev-read+ le:+ev-write+))

      ;; depending on what object we passed in (socket/stream) return the SSL
      ;; equivalent
      (if passed-in-stream-p
          ssl-tcp-stream
          ssl-socket))))

(defun tcp-ssl-server (bind-address port read-cb event-cb
                       &key connect-cb (backlog -1) stream
                            certificate key password)
  "Start a TCP listener, and wrap incoming connections in an SSL handler.
   Returns a tcp-server object, which can be closed with close-tcp-server.

   If you need a self-signed cert/key to test with:
     openssl genrsa -out pkey 2048
     openssl req -new -key pkey -out cert.req
     openssl x509 -req -days 3650 -in cert.req -signkey pkey -out cert"
  ;; make sure SSL is initialized
  (cl+ssl:ensure-initialized :method 'cl+ssl::ssl-v23-server-method)

  ;; create the server and grab its data-pointer
  (let* ((server (tcp-server bind-address port
                             read-cb event-cb
                             :connect-cb connect-cb
                             :backlog backlog
                             :stream stream))
         (data-pointer (tcp-server-c server)))
    ;; overwrite the accept callback from tcp-accept-cb -> tcp-ssl-accept-cb
    (le:evconnlistener-set-cb (tcp-server-c server)
                              (cffi:callback tcp-ssl-accept-cb)
                              data-pointer)
    ;; create a server context
    (let* ((ssl-ctx (cl+ssl::ssl-ctx-new (cl+ssl::ssl-v23-server-method)))
           (ssl-server (change-class server 'tcp-ssl-server :ssl-ctx ssl-ctx)))
      ;; make sure if there is a cert password, it's used
      (cl+ssl::with-pem-password (password)
        (cl+ssl::ssl-ctx-set-default-passwd-cb ssl-ctx (cffi:callback cl+ssl::pem-password-callback))

        ;; load the cert
        (when certificate
          (let ((res (cffi:foreign-funcall "SSL_CTX_use_certificate_chain_file"
                                           :pointer ssl-ctx
                                           :string (namestring certificate)
                                           :int)))
            (unless (= res 1)
              (error (format nil "Error initializing certificate: ~a."
                             (last-ssl-error))))))

        ;; load the private key
        (when key
          (let ((res (cffi:foreign-funcall "SSL_CTX_use_PrivateKey_file"
                                           :pointer ssl-ctx
                                           :string (namestring key)
                                           :int cl+ssl::+ssl-filetype-pem+
                                           :int)))
            (unless (= res 1)
              (error (format nil "Error initializing private key file: ~a."
                             (last-ssl-error)))))))

      ;; adjust the data-pointer's data a bit
      (attach-data-to-pointer data-pointer
                              (list :server server
                                    :ctx ssl-ctx))
      ssl-server)))








;;; ----------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------


(defun wrap-in-ssl (socket/stream &key (ssl-ctx cl+ssl::*ssl-global-context*))
  "Given a cl-async:socket object, wrap it in an SSL layer."
  (cl+ssl:ensure-initialized :method 'cl+ssl::ssl-v23-client-method)
  (let* ((global-ctx cl+ssl::*ssl-global-context*)
         (socket (if (typep socket/stream 'as:async-stream)
                     (as:stream-socket socket/stream)
                     socket/stream))
         (uvstream (socket-c socket))
         (ssl-ctx (cl+ssl::ssl-ctx-new (cl+ssl::ssl-v23-client-method))
         

(defun tcp-ssl-connect (host port read-cb event-cb
                        &key data stream ssl-ctx
                        connect-cb write-cb
                        (read-timeout -1) (write-timeout -1)
                        (dont-drain-read-buffer nil dont-drain-read-buffer-supplied-p))
  "Just like cl-async:tcp-connect, but wraps the socket in SSL."
  (let ((sock (apply 'as:tcp-connect
                     (append (list host port read-cb event-cb
                                   :data data
                                   :stream stream
                                   :connect-cb connect-cb
                                   :write-cb write-cb
                                   :read-timeout read-timeout
                                   :write-timeout write-timeout)
                             (when dont-drain-read-buffer-supplied-p
                               (list :dont-drain-read-buffer dont-drain-read-buffer))))))
    (wrap-in-ssl sock :ssl-ctx ssl-ctx)))
|#

(defun tcp-ssl-server (bind-address port read-cb event-cb
                       &key connect-cb (backlog -1) stream
                            certificate key password))

