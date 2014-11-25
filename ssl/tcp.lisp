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
  (setf (as-ssl-freed as-ssl) t)
  (let* ((ctx (as-ssl-ctx as-ssl))
         (ssl (as-ssl-ssl as-ssl)))
    (when (cffi:pointerp ctx)
      (ssl-ctx-free ctx))
    (when (cffi:pointerp ssl)
      (free-pointer-data ssl :preserve-pointer t)
      (ssl-free ssl))))
  
(defmethod close-socket ((socket ssl-socket) &key force)
  (declare (ignore force))
  (let* ((as-ssl (socket-as-ssl socket)))
    (ssl-shutdown (as-ssl-ssl as-ssl))
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
  (format t "< write: (encrypted? ~a) ~a~%" (not write-ssl) (length (subseq data (or start 0) end)))
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
  "This function stupidly loops over SSL's objects looking for things to do,
   such as copying data from the write BIO into the read BIO or checking it
   SSL can be read from, etc. this is necessary because SSL doesn't really have
   any mechanism for triggering events when certain actions can be performed, so
   we just blunder through it each time *anything happens*."
  (let* ((data (deref-data-from-pointer ssl))
         (socket (getf data :socket))
         (stream (getf data :stream))
         (drain-read (socket-drain-read-buffer socket))
         (callbacks (get-callbacks (as:socket-c socket)))
         (read-cb (getf callbacks :read-cb))
         ;(event-cb (getf callbacks :event-cb))
         (as-ssl (socket-as-ssl socket))
         (bio-write (as-ssl-bio-write as-ssl))
         (written 0))
    (loop for nread = (ssl-bio-read bio-write (static-vectors:static-vector-pointer *output-buffer*) (length *output-buffer*))
          while (< 0 nread) do
      (write-socket-data socket *output-buffer* :end nread)
      (incf written nread))
    (if (ssl-is-init-finished ssl)
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
                   (format t "! err(~a): ~a~%" err str)))
                ((and read-cb drain-read)
                 (funcall read-cb socket (subseq *input-buffer* 0 ret)))
                (stream
                 (stream-append-bytes stream (subseq *input-buffer* 0 ret))
                 (when read-cb (funcall read-cb socket stream)))))
        (ssl-connect ssl))))

(define-c-callback tcp-ssl-info-cb :void ((ssl :pointer) (where :int) (ret :int))
  "Called whenever *stuff* happens on an SSL object."
  (let ((w (logand where +ssl-st-mask+))
        (state (ssl-state-string-long ssl)))
    (format t ". info: ~a ~s~%" state (list where ret))
    (cond ((& w +ssl-st-connect+) )
          ((& w +ssl-st-accept+) )
          ((& w +ssl-cb-handshake-done+) )
          ((& w +ssl-cb-handshake-start+) )
          ((& w +ssl-cb-read+) )
          ((& w +ssl-cb-write+) )
          ((& w +ssl-cb-alert+)
           (let ((read-alert (& w +ssl-cb-read+))
                 (err (ssl-alert-type-string-long ret))
                 (desc (ssl-alert-desc-string-long ret)))
             (format t "! ssl: alert: (~s): ~a / ~a~%" (if read-alert :read :write) err desc)))
          ((& w +ssl-cb-exit+) )
          ((& w +ssl-cb-loop+) ;(ssl-run-state ssl)
                               ))))

(defun tcp-ssl-connect (host port read-cb event-cb &key data stream connect-cb write-cb (read-timeout -1) (write-timeout -1) (dont-drain-read-buffer nil dont-drain-read-buffer-supplied-p))
  "Create and return an SSL-activated socket."
  (check-event-loop-running)
  (labels ((connect-cb (sock)
             (ssl-run-state (as-ssl-ssl (socket-as-ssl sock)))
             (when connect-cb
               (funcall connect-cb sock)))
           (read-cb (sock data)
             (format t "> read: (encrypted? ~a) ~a~%" t (length data))
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
        (attach-data-to-pointer ssl (list :socket socket
                                          :stream stream))
        (setf (socket-as-ssl socket) (make-instance 'as-ssl
                                                    :ctx ctx
                                                    :ssl ssl
                                                    :bio-read bio-read
                                                    :bio-write bio-write))
        (ssl-connect ssl))
      socket/stream)))

(defun tcp-ssl-server (bind-address port read-cb event-cb
                       &key connect-cb (backlog -1) stream
                            certificate key password))

#|
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
|#

