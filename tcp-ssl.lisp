(defpackage :cl-async-ssl
  (:use :cl :cl-async :cl-async-util)
  (:nicknames :as-ssl)
  (:export #:ssl-socket
           #:socket-underlying
           #:tcp-ssl-error
           ;#:wrap-in-ssl
           #:tcp-ssl-connect
           ;#:tcp-ssl-server ; TODO: enable once tests are written
           )
  (:import-from :cl-async
                #:socket-drain-read-buffer
                #:write-to-evbuffer
                #:tcp-read-cb
                #:tcp-write-cb
                #:tcp-event-cb
                #:init-incoming-socket
                #:tcp-server-c
                #:tcp-server-data-pointer))
(in-package :cl-async-ssl)

(define-condition tcp-ssl-error (tcp-error) ()
  (:report (lambda (c s) (format s "SSL connection error: ~a: ~a" (event-errcode c) (event-errmsg c))))
  (:documentation "Describes a general SSL connection error."))

;; cl+ssl doesn't wrap these (bummer) so we have to do it manually...
(defconstant +ssl-received-shutdown+ 2)  ; #define SSL_RECEIVED_SHUTDOWN   2
(cffi:defcfun ("SSL_set_shutdown" ssl-set-shutdown) :void
  (ssl :pointer)
  (mode :int))
(cffi:defcfun ("SSL_shutdown" ssl-shutdown) :int
  (ssl :pointer))

(defclass ssl-socket (socket)
  ((ssl-ctx :accessor socket-ssl-ctx :initarg :ctx :initform nil)
   (underlying :accessor socket-underlying :initarg :underlying :initform nil
     :documentation "Stores the original socket that was wrapped in SSL.")
   (close-cb :accessor socket-close-cb :initarg :close-cb :initform nil))
  (:documentation "Wraps a libevent SSL socket."))

(defmethod close-socket ((socket ssl-socket))
  "Closes and frees an SSL socket."
  ;; close the SSL context and call the close callback
  (let ((ctx (socket-ssl-ctx socket))
        (close-cb (socket-close-cb socket)))
    (when (and (cffi:pointerp ctx)
               (not (cffi:null-pointer-p ctx)))
      ;; according to http://www.wangafu.net/~nickm/libevent-book/Ref6a_advanced_bufferevents.html,
      ;; this is the most performant way to close SSL connections (must be done
      ;; before freeing the context/bev).
      (ssl-set-shutdown ctx +ssl-received-shutdown+)
      (ssl-shutdown ctx))
    (when close-cb
      (funcall close-cb)))
  ;; this will free the bufferevent/any SSL resources and close the connection.
  ;; the underlying bufferevent will be freed by libevent automatically when the
  ;; SSL bufferevent is freed, so no need to clean this up manually.
  (call-next-method))

(defun last-ssl-error ()
  "Returns the last error string (nil if none) and the last error code that
   happened in SSL land."
  (let ((errcode (cffi:foreign-funcall "ERR_get_error" :int)))
    (values
      (cffi:foreign-funcall "ERR_reason_error_string"
                            :int errcode
                            :string)
      errcode)))

(cffi:defcallback ssl-event-cb :void ((bev :pointer) (events :short) (data-pointer :pointer))
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
                  (run-event-cb event-cb (make-instance 'tcp-ssl-error :code errcode :msg str)))))))
        ;; call directly into the tcp-event-cb function (if we haven't already
        ;; dealt with the error)
        (unless ssl-error
          (cffi:foreign-funcall-pointer
            (cffi:callback tcp-event-cb) ()
            :pointer bev :short events :pointer data-pointer))))))

(cffi:defcallback tcp-ssl-accept-cb :void ((listener :pointer) (fd :int) (addr :pointer) (socklen :int) (data-pointer :pointer))
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

(defun init-tcp-ssl-socket (ssl-ctx read-cb event-cb &key data stream (fd -1) connect-cb write-cb (read-timeout -1) (write-timeout -1) (dont-drain-read-buffer nil dont-drain-read-buffer-supplied-p))
  "Initialize an async SSL socket, but do not connect it."
  (check-event-loop-running)

  ;; make sure we have a context
  (unless (cffi:pointerp ssl-ctx)
    (setf ssl-ctx cl+ssl::*ssl-global-context*))

  (let* ((data-pointer (create-data-pointer))
         (fd (or fd -1))
         (bev (le-ssl:bufferevent-openssl-socket-new *event-base* fd ssl-ctx (cffi:foreign-enum-value 'le-ssl:bufferevent-ssl-state ':bufferevent-ssl-connecting) +bev-opt-close-on-free+))

         ;; assume dont-drain-read-buffer if unspecified and requesting a stream
         (dont-drain-read-buffer (if (and stream (not dont-drain-read-buffer-supplied-p))
                                     t
                                     dont-drain-read-buffer))
         (socket (make-instance 'ssl-socket :c bev
                                            :direction 'out
                                            :ctx ssl-ctx
                                            :drain-read-buffer (not dont-drain-read-buffer)))
         (tcp-stream (when stream (make-instance 'async-io-stream :socket socket))))

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

(defun init-ssl-client-context (global-ctx)
  "Initialize an SSL client context."
  (let ((client-ctx (cl+ssl::ssl-new global-ctx)))
    (cl+ssl::ssl-ctx-ctrl client-ctx
                          cl+ssl::+SSL_CTRL_MODE+ 
                          cl+ssl::+SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER+
                          0)
    (cl+ssl::ssl-set-connect-state client-ctx)
    (when (zerop (cl+ssl::ssl-set-cipher-list client-ctx "ALL"))
      (error 'cl+ssl::ssl-error-initialize :reason "Can't set SSL cipher list"))
    ;; make sure we init PROPERLY
    (when (cffi:null-pointer-p client-ctx)
      (error "Problem initializing SSL context."))
    client-ctx))
    
(defun tcp-ssl-connect (host port read-cb event-cb &key data stream connect-cb write-cb (read-timeout -1) (write-timeout -1) (dont-drain-read-buffer nil dont-drain-read-buffer-supplied-p) ssl-ctx)
  "Open a TCP connection asynchronously. Optionally send data out once connected
   via the :data keyword (can be a string or byte array)."
  ;; make sure SSL is ready to go
  (cl+ssl:ensure-initialized :method 'cl+ssl::ssl-v23-client-method)
  (let* ((ssl-ctx (or ssl-ctx cl+ssl::*ssl-global-context*))
         (client-ctx (init-ssl-client-context ssl-ctx)))
    (let* ((socket/stream (apply #'init-tcp-ssl-socket
                                 (append (list client-ctx read-cb event-cb
                                               :data data
                                               :stream stream
                                               :connect-cb connect-cb
                                               :write-cb write-cb
                                               :read-timeout read-timeout
                                               :write-timeout write-timeout)
                                         (when dont-drain-read-buffer-supplied-p
                                           (list :dont-drain-read-buffer dont-drain-read-buffer))))))
      (connect-tcp-socket socket/stream host port)
      socket/stream)))

;; TODO: Figure out why read/write timeouts are borked when wrapping an existing
;; bufferevent. Until then, this function should be treated with caution (ie,
;; not exposed in the API)
(defun wrap-in-ssl (socket/stream
                     &key (ssl-context cl+ssl::*ssl-global-context*) close-cb
                     certificate key password)
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
                      *event-base*
                      bufferevent-orig  ; pass in the original bev
                      ssl-ctx
                      state
                      ;; yes, close on free
                      (cffi:foreign-enum-value 'le:bufferevent-options ':+bev-opt-close-on-free+)))
           ;; make an SSL socket that matches the options of the original socket
           (ssl-socket (make-instance 'ssl-socket
                                      :ctx ssl-ctx
                                      :c ssl-bev
                                      :close-cb close-cb
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
         (data-pointer (tcp-server-data-pointer server)))
    ;; overwrite the accept callback
    (le:evconnlistener-set-cb (tcp-server-c server)
                              (cffi:callback tcp-ssl-accept-cb)
                              data-pointer)
    (let ((ssl-ctx (cl+ssl::ssl-ctx-new (cl+ssl::ssl-v23-server-method))))
      ;; make sure if there is a cert password, it's used
      (cl+ssl::with-pem-password (password)
        (cl+ssl::ssl-ctx-set-default-passwd-cb ssl-ctx (cffi:callback cl+ssl::pem-password-callback))

        ;; load the cert
        (when certificate
          (let ((res (cffi:foreign-funcall "SSL_CTX_use_certificate_chain_file"
                                           :pointer ssl-ctx
                                           :string certificate
                                           :int)))
            (unless (= res 1)
              (error (format nil "Error initializing certificate: ~a."
                             (last-ssl-error))))))

        ;; load the private key
        (when key
          (let ((res (cffi:foreign-funcall "SSL_CTX_use_PrivateKey_file"
                                           :pointer ssl-ctx
                                           :string key
                                           :int cl+ssl::+ssl-filetype-pem+
                                           :int)))
            (unless (= res 1)
              (error (format nil "Error initializing private key file: ~a."
                             (last-ssl-error)))))))

      ;; adjust the data-pointer's data a bit
      (attach-data-to-pointer data-pointer
                              (list :server server
                                    :ctx ssl-ctx)))))

