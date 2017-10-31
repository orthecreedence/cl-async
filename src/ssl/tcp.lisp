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
   (ssl-buffer :accessor socket-ssl-buffer :initform (make-buffer))
   (ssl-connected :accessor socket-ssl-connected :initform nil)
   (ssl-closing :accessor socket-ssl-closing :initform nil)
   (ssl-function :accessor socket-ssl-function :initarg :function :initform 'ssl-connect))
  (:documentation "Extends cl-async's socket to hold SSL info"))

(defclass tcp-ssl-server (tcp-server)
  ((as-ssl :accessor tcp-server-as-ssl :initarg :as-ssl :initform nil))
  (:documentation "Wraps around an SSL server/listener"))

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *ssl-init* nil
    "Holds whether or not we've been init.")

  (defun ensure-init (&key from-load)
    (unless *ssl-init*
      (cffi:foreign-funcall ("SSL_library_init") :void)
      (cffi:foreign-funcall ("SSL_load_error_strings") :void)
      (cffi:foreign-funcall ("ERR_load_BIO_strings") :void)
      (unless from-load
        (setf *ssl-init* t)))))

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

(defmethod close-streamish ((socket ssl-socket) &key force)
  (declare (ignore force))
  (setf (socket-ssl-closing socket) t)
  (let* ((as-ssl (socket-as-ssl socket))
         (ssl (as-ssl-ssl as-ssl)))
    (call-next-method)
    (ssl-shutdown ssl)
    (close-ssl as-ssl)))

(defmethod close-socket-server ((tcp-server tcp-ssl-server))
  ;; shut down the listener before freeing the SSL handles
  (call-next-method)
  ;; free the SSL ctx (if it exists)
  (close-ssl (tcp-server-as-ssl tcp-server)))

(defun write-to-ssl (as-ssl data)
  "Write data into SSL."
  (let ((data (if (stringp data)
                  (babel:string-to-octets data :encoding :utf-8)
                  data))
        (buff *output-buffer*)
        (ssl (as-ssl-ssl as-ssl)))
    (do-chunk-data data buff
      (lambda (buffer bufsize)
        (ssl-write ssl (static-vectors:static-vector-pointer buffer) bufsize)))
    ;; zero that shit
    (zero-buffer buff)
    (ssl-run-state ssl)))

(defmethod streamish-write ((socket ssl-socket) data &key read-cb write-cb event-cb start end force (write-ssl t))
  (declare (ignore force))
  (cond ((and (not (socket-ssl-connected socket))
              write-ssl)
         ;; we're writing data INTO ssl (unencrypted data) but we haven't
         ;; finished connecting to SSL yet, so chill on the writing (just buffer
         ;; it)
         (let ((data (if (stringp data)
                         (babel:string-to-octets data :encoding :utf-8)
                         data)))
           (vom:debug "< write: buffer: ~a~%" (length (subseq data (or start 0) end)))
           (write-to-buffer data (socket-ssl-buffer socket) start end)
           (return-from streamish-write nil)))
        ((not write-ssl)
         ;; we're writing raw socket data (encrypted data), so blast it out
         (vom:debug "< write: raw: ~a (conn ~a)~%" (length (subseq data (or start 0) end)) (socket-connected socket))
         (return-from streamish-write (call-next-method))))
  (vom:debug "< write: ssl: ~a~%" (length (subseq data (or start 0) end)))
  (let* ((uvstream (socket-c socket))
         (as-ssl (socket-as-ssl socket))
         (do-send (lambda ()
                    (write-to-ssl as-ssl data))))
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

(defmethod write-pending-socket-data ((socket ssl-socket))
  ;; write out raw bytes using :write-ssl nil
  (let ((pending (buffer-output (socket-buffer socket))))
    (setf (socket-buffer socket) (make-buffer))
    (write-socket-data socket pending :force t :write-ssl nil))

  ;; if the handshake is done, write our SSL bytes
  (when (socket-ssl-connected socket)
    (let ((pending (buffer-output (socket-ssl-buffer socket))))
      (setf (socket-ssl-buffer socket) (make-buffer))
      (write-socket-data socket pending :force t)
      (zero-buffer pending))))

(defun ssl-run-state (ssl &key from-info)
  "This function stupidly loops over SSL's objects looking for things to do,
   such as copying data from the write BIO into the read BIO or checking it
   SSL can be read from, etc. this is necessary because SSL doesn't really have
   any mechanism for triggering events when certain actions can be performed, so
   we just blunder through it each time *anything happens*."
  (let* ((data (let ((data (deref-data-from-pointer ssl)))
                 (unless data (return-from ssl-run-state))
                 data))
         (socket (getf data :socket))
         (stream (getf data :stream))
         (drain-read (socket-drain-read-buffer socket))
         (callbacks (get-callbacks (as:socket-c socket)))
         (read-cb (getf callbacks :ssl-read-cb))
         (event-cb (getf callbacks :event-cb))
         (as-ssl (socket-as-ssl socket))
         (bio-write (as-ssl-bio-write as-ssl)))
    (catch-app-errors event-cb
      (when (and from-info (zerop (ssl-bio-ctrl-pending bio-write)))
        (return-from ssl-run-state))
      (when (< 0 (ssl-bio-ctrl-pending bio-write))
        (let ((buff *output-buffer*))
          (loop for nread = (ssl-bio-read bio-write (static-vectors:static-vector-pointer buff) (length buff))
                while (< 0 nread) do
            (write-socket-data socket buff :end nread :write-ssl nil))))
      (if (ssl-is-init-finished ssl)
          (progn
            (unless (socket-ssl-connected socket)
              (vom:debug ". init, run buffered SSL writes~%")
              (setf (socket-ssl-connected socket) t)
              ;; write any buffered output we've stored up
              (write-pending-socket-data socket))
            ;; make sure we leave if the socket is shutting down (which can be
            ;; triggered in the (write-pending-socket-data ...) call above)
            (when (socket-ssl-closing socket)
              (return-from ssl-run-state))
            (let* ((buff *input-buffer*)
                   (ret (ssl-read ssl (static-vectors:static-vector-pointer buff) (length buff))))
              (cond ((<= ret 0)
                     (let* ((err (ssl-get-error ssl ret))
                            (code nil)
                            (str (cond
                                   ((= err +ssl-error-ssl+)
                                    (setf code (ssl-err-get-error))
                                    (ssl-err-error-string code (cffi:null-pointer)))
                                   ((= err +ssl-error-want-read+)
                                     "want-read")
                                   ((= err +ssl-error-want-write+)
                                    "want-write")
                                   ((= err +ssl-error-want-accept+)
                                    "want-accept")
                                   ((= err +ssl-error-want-connect+)
                                    "want-connect"))))
                       (if (find err (list +ssl-error-ssl+))
                           (run-event-cb 'event-handler
                                         (make-instance 'tcp-ssl-error
                                                        :code code
                                                        :msg str
                                                        :socket socket)
                                         event-cb)
                           (vom:debug "! err(~a): ~a~%" err str))))
                    ((and (< 0 ret) read-cb drain-read)
                     (funcall read-cb socket (subseq buff 0 ret))
                     (ssl-run-state ssl))
                    ((and (< 0 ret) stream)
                     (stream-append-bytes stream (subseq buff 0 ret))
                     (when read-cb (funcall read-cb socket stream))
                     (ssl-run-state ssl)))
              ;; zero that shit
              (zero-buffer buff)))
          (funcall (socket-ssl-function socket) ssl)))))

(define-c-callback tcp-ssl-info-cb :void ((ssl :pointer) (where :int) (ret :int))
  "Called whenever *stuff* happens on an SSL object. We use this to run our SSL
   state function sometimes."
  (let ((w (logand where +ssl-st-mask+))
        (state (ssl-state-string-long ssl)))
    (vom:debug ". info: ~a ~s~%" state (list where ret))
    (cond ((& w +ssl-st-connect+))
          ((& w +ssl-st-accept+))
          ((& w +ssl-cb-handshake-done+)
           (vom:debug "* handshake~%")
           ;(ssl-run-state ssl :from-info t)
           )
          ((& w +ssl-cb-handshake-start+))
          ((& w +ssl-cb-read+))
          ((& w +ssl-cb-write+))
          ((& w +ssl-cb-alert+)
           (let ((read-alert (& w +ssl-cb-read+))
                 (err (ssl-alert-type-string-long ret))
                 (desc (ssl-alert-desc-string-long ret)))
             (vom:debug "! ssl: alert: (~s): ~a / ~a~%" (if read-alert :read :write) err desc)))
          ((& w +ssl-cb-exit+)
           (vom:debug "* exit~%")
           ;(ssl-run-state ssl :from-info t)
           )
          ((& w +ssl-cb-loop+)))))

(defun create-ssl-ctx (&key (method :sslv23) options)
  "Simplifies some common CTX setup stuff."
  (ensure-init)
  (let ((ctx (ssl-ctx-new (case method
                            (:sslv23-client (ssl-sslv23-client-method))
                            (:sslv23-server (ssl-sslv23-server-method))
                            (:tlsv1-client (ssl-tlsv1-client-method))
                            (:tlsv1-server (ssl-tlsv1-server-method))
                            (:tlsv1 (ssl-tlsv1-method))
                            (t (ssl-sslv23-method)))))
        (options (logior +ssl-op-all+
                         (or options
                             ;; default is more secure (TLS >= 1)
                             (logior +ssl-op-no-sslv2+
                                     +ssl-op-no-sslv3+)))))
    (when (cffi:null-pointer-p ctx)
      (let* ((errcode (ssl-err-get-error))
             (str (ssl-err-reason-error-string errcode)))
        (error (make-instance 'tcp-ssl-error :code errcode :msg (format nil "error creating SSL context: ~a" str)))))
    (ssl-ctx-set-options ctx options)
    ctx))

(defun attach-ssl-to-socket (ctx socket/stream connect-cb after-create-cb &key store-ctx ciphers)
  "Given a normal cl-async socket (incoming or outgoing), set it up to be
   wrapped in SSL by attaching some SSL objects to it and replacing the
   connect-cb and read-cb with our own. Note that the given socket MUST be of
   the class ssl-socket.

   Note that this function *really* should be called before the passed socket
   is connected, and certainly before any data is exchanged over it."
  (labels ((read-cb (sock data)
             (let* ((as-ssl (socket-as-ssl sock))
                    (ssl (as-ssl-ssl as-ssl))
                    (bio-read (as-ssl-bio-read as-ssl))
                    (buff *input-buffer*))
               (do-chunk-data data buff
                 (lambda (buffer bufsize)
                   (vom:debug "> read: raw: ~a~%" (length buffer))
                   (ssl-bio-write bio-read (static-vectors:static-vector-pointer buffer) bufsize)))
               (ssl-run-state ssl)
               (as:with-delay () (ssl-run-state ssl)))))
    (let* ((socket (if (typep socket/stream 'as:async-stream)
                       (as:stream-socket socket/stream)
                       socket/stream))
           (socket (change-class socket 'ssl-socket))
           (stream (when (typep socket/stream 'as:async-stream) socket/stream))
           (socket-c (as:socket-c socket))
           (callbacks (get-callbacks socket-c))
           (ciphers (or ciphers
                        "HIGH:!RC4:!MD5:!aNULL:!EDH:!EXP:+ECDHE-RSA-AES128-SHA256:+3DES")))
      ;; replace our socket's callbacks
      (setf (getf callbacks :connect-cb) connect-cb)
      (setf (getf callbacks :ssl-read-cb) (getf callbacks :read-cb))
      (setf (getf callbacks :read-cb) #'read-cb)
      (save-callbacks socket-c callbacks)
      (let ((ssl (ssl-new ctx))
            (bio-read (ssl-bio-new (ssl-bio-s-mem)))
            (bio-write (ssl-bio-new (ssl-bio-s-mem))))
        (funcall after-create-cb ssl)
        (ssl-set-cipher-list ssl ciphers)
        (ssl-set-bio ssl bio-read bio-write)
        (ssl-set-info-callback ssl (cffi:callback tcp-ssl-info-cb))
        ;(ssl-bio-set-mem-eof-return bio-read -1)
        ;(ssl-bio-set-mem-eof-return bio-write -1)
        (attach-data-to-pointer ssl (list :socket socket
                                          :stream stream))
        (setf (socket-as-ssl socket) (make-instance 'as-ssl
                                                    :ctx (when store-ctx ctx)
                                                    :ssl ssl
                                                    :bio-read bio-read
                                                    :bio-write bio-write))
        socket/stream))))

(defun tcp-ssl-connect-new (host port read-cb &key data stream event-cb connect-cb write-cb (read-timeout -1) (write-timeout -1) (dont-drain-read-buffer nil dont-drain-read-buffer-supplied-p) ssl-ctx ssl-options ciphers)
  "Create and return an SSL-activated socket."
  (check-event-loop-running)
  (let* ((socket/stream (apply #'as:tcp-connect
                               (append (list host port
                                             read-cb
                                             :event-cb event-cb
                                             ;; we pass nil data otherwise it will be sent out
                                             ;; raw when we connect (bad)
                                             :data nil
                                             :stream stream
                                             :connect-cb connect-cb
                                             :write-cb write-cb
                                             :read-timeout read-timeout
                                             :write-timeout write-timeout)
                                       (when dont-drain-read-buffer-supplied-p
                                         (list :dont-drain-read-buffer dont-drain-read-buffer)))))
         (socket (if (typep socket/stream 'as:async-stream)
                     (stream-socket socket/stream)
                     socket/stream))
         (ctx (or ssl-ctx
                  (let ((ctx (create-ssl-ctx :options ssl-options)))
                    (ssl-ctx-set-default-verify-paths ctx)
                    ;; TODO better verify support
                    (ssl-ctx-set-verify ctx +ssl-verify-none+ (cffi:null-pointer))
                    ctx))))
    (attach-ssl-to-socket ctx socket/stream
                          (lambda (sock/stream)
                            (let ((sock (if (typep sock/stream 'as:async-stream)
                                            (as:stream-socket sock/stream)
                                            sock/stream)))
                              (setf (socket-ssl-function sock) 'ssl-connect)
                              (let ((ssl (as-ssl-ssl (socket-as-ssl sock))))
                                (ssl-connect ssl)
                                (ssl-run-state ssl))
                              (when connect-cb
                                (funcall connect-cb sock/stream))))
                          (lambda (ssl)
                            (ssl-set-connect-state ssl))
                          :store-ctx (not ssl-ctx)
                          :ciphers ciphers)
    ;; now that the 'socket class was replaced with 'ssl-socket, we can safely
    ;; write out our data and it will be buffered properly.
    (when data
      (write-socket-data socket data))
    socket/stream))

(defun tcp-ssl-connect (host port read-cb &rest args)
  "Open a TCP connection asynchronously. Optionally send data out once connected
   via the :data keyword (can be a string or byte array)."
  (let ((event-cb-dep (car args)))
    (unless (or (keywordp event-cb-dep)
                (null event-cb-dep))
      (push :event-cb args)
      (warn "Passing event-cb as the fourth argument to tcp-ssl-connect is now deprecated. Please use the :event-cb keyword instead."))
    (apply 'tcp-ssl-connect-new
           host port read-cb
           args)))

(defun tcp-ssl-server-new (bind-address port read-cb
                            &key event-cb connect-cb (backlog -1) stream fd
                                 ssl-ctx
                                 certificate key (keytype :pem) ssl-options ciphers)
  "Wraps a tcp server in SSL."
  (let* ((ctx (or ssl-ctx
                  (let ((ctx (create-ssl-ctx :method :sslv23-server :options ssl-options)))
                    (when certificate
                      (let ((res (ssl-ctx-use-certificate-chain-file ctx (namestring certificate))))
                        (when (<= res 0)
                          (let* ((code (ssl-err-get-error))
                                 (msg (ssl-err-error-string code (cffi:null-pointer))))
                            (error 'tcp-ssl-error :code code :msg (format nil "error opening certificate(~a): ~a (~a)~%" code msg certificate))))))
                    (when key
                      (let* ((type (case keytype
                                     (:pem +ssl-filetype-pem+)
                                     (:asn1 +ssl-filetype-asn1+)
                                     (t +ssl-x509-filetype-default+)))
                             (res (ssl-ctx-use-privatekey-file ctx (namestring key) type)))
                        (when (<= res 0)
                          (let* ((code (ssl-err-get-error))
                                 (msg (ssl-err-error-string code (cffi:null-pointer))))
                            (error 'tcp-ssl-error :code code :msg (format nil "error loading keyfile(~a): ~a (~a)~%" code msg key))))))
                    ctx)))
         (wrap-connect-cb (lambda (sock)
                            (attach-ssl-to-socket ctx sock 'ssl-accept
                              (lambda (ssl) (ssl-set-accept-state ssl))
                              ;; we don't want to store the CTX because doing so
                              ;; causes it to be freed when the socket closes,
                              ;; and we can't have our sockets closing our
                              ;; server's CTX willy nilly.
                              :store-ctx nil
                              :ciphers ciphers)
                            (setf (socket-ssl-function sock) 'ssl-accept)
                            (let ((ssl (as-ssl-ssl (socket-as-ssl sock))))
                              (ssl-accept ssl)
                              (ssl-run-state ssl))
                            (when connect-cb (funcall connect-cb sock))))
         (server (as:tcp-server bind-address
                                port
                                read-cb
                                event-cb
                                :connect-cb wrap-connect-cb
                                :backlog backlog
                                :stream stream
                                :fd fd)))
    (change-class server 'tcp-ssl-server)
    ;; notice that if we're using a passed-in context, we don't add it to the
    ;; as-ssl object. this means it won't be freed when the server closes.
    ;; however if ssl-ctx is nil, it means we created our own context and it
    ;; will be freed on close.
    (let ((as-ssl (make-instance 'as-ssl :ctx (unless ssl-ctx ctx))))
      (setf (tcp-server-as-ssl server) as-ssl))
    server))

(defun tcp-ssl-server (bind-address port read-cb &rest args)
  "Open a TCP connection asynchronously. Optionally send data out once connected
   via the :data keyword (can be a string or byte array)."
  (let ((event-cb-dep (car args)))
    (unless (or (keywordp event-cb-dep)
                (null event-cb-dep))
      (push :event-cb args)
      (warn "Passing event-cb as the fourth argument to tcp-ssl-server is now deprecated. Please use the :event-cb keyword instead."))
    (apply 'tcp-ssl-server-new
           bind-address port read-cb
           args)))

(eval-when (:compile-toplevel :load-toplevel)
  (ensure-init :from-load t))
