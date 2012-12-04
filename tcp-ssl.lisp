(defpackage :cl-async-ssl
  (:use :cl :cl-async :cl-async-util)
  (:nicknames :as-ssl)
  (:export #:ssl-socket
           #:tcp-ssl-error
           #:wrap-in-ssl))
(in-package :cl-async-ssl)

(define-condition tcp-ssl-error (tcp-error) ()
  (:report (lambda (c s) (format s "SSL connection error: ~a: ~a" (conn-errcode c) (conn-errmsg c))))
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

(cffi:defcallback ssl-event-cb :void ((bev :pointer) (events :short) (data-pointer :pointer))
  "Called whenever anything happens on a TCP socket. Ties into the anonymous
   callback system to track failures/disconnects."
  (let* ((bev-data (deref-data-from-pointer bev))
         (socket (getf bev-data :socket))
         (event-cb (getf (get-callbacks data-pointer) :event-cb)))
    (catch-app-errors event-cb
      ;; look specifically for SSL errors
      (let ((errcode (le-ssl:bufferevent-get-openssl-error bev)))
        (unless (zerop errcode)
          (setf events le:+bev-event-error+)
          ;; we found an error. grab the errstring and call the event-cb
          (cffi:with-foreign-object (buf :unsigned-char 256)
            (cl+ssl::err-error-string errcode buf)
            (let ((str (cffi:foreign-string-to-lisp buf)))
              (funcall event-cb (make-instance 'tcp-ssl-error :code errcode :msg str))))
          ;; make sure to close the socket after an error
          (close-socket socket))
        ;; call directly into the tcp-event-cb function
        (cffi:foreign-funcall-pointer
          (cffi:callback as::tcp-event-cb) ()
          :pointer bev :short events :pointer data-pointer)))))

(defun wrap-in-ssl (socket/stream &key certificate key password (method 'cl+ssl::ssl-v23-client-method) close-cb)
  "Wraps a cl-async socket/stream in the SSL protocol using libevent's SSL
   capabilities. Does some trickery when swapping out the event function for the
   new socket, but the original event-cb will still be called."
  (declare (ignore close-cb))
  (cl+ssl:ensure-initialized :method method)  ; make sure SSL is ready to go

  (let* ((global-ctx cl+ssl::*ssl-global-context*)
         (passed-in-stream-p (subtypep (type-of socket/stream) 'async-stream))
         ;; whether we got a socket or a stream, grab a socket
         (socket (if passed-in-stream-p
                     (stream-socket socket/stream)
                     socket/stream))
         (bufferevent-orig (socket-c socket))
         ;; create an SSL client oontext
         (ssl-ctx (cl+ssl::ssl-new global-ctx)))
    (cl+ssl::ssl-ctx-ctrl ssl-ctx
                          cl+ssl::+SSL_CTRL_MODE+ 
                          cl+ssl::+SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER+
                          0)
    (cl+ssl::ssl-set-connect-state ssl-ctx)
    (cl+ssl::with-pem-password (password)
      (cl+ssl::install-key-and-cert ssl-ctx key certificate))
    ;; make sure we init PROPERLY
    (when (cffi:null-pointer-p ssl-ctx)
      (error "Problem initializing SSL context."))

    ;; create our SSL socket/stream and make sure it grabs any callbacks/hooks
    ;; it needs to operate properly from the original bufferevent that tcp-send
    ;; created.
    (let* ((ssl-bev (le-ssl:bufferevent-openssl-filter-new
                      *event-base*
                      bufferevent-orig  ; pass in the original bev
                      ssl-ctx
                      ;; we're connecting
                      (cffi:foreign-enum-value 'le-ssl:bufferevent-ssl-state ':bufferevent-ssl-connecting)
                      ;; yes, close on free
                      (cffi:foreign-enum-value 'le:bufferevent-options ':+bev-opt-close-on-free+)))
           ;; make an SSL socket that matches the options of the original socket
           (ssl-socket (make-instance 'ssl-socket
                                      :ctx ssl-ctx
                                      :c ssl-bev
                                      :drain-read-buffer (as::socket-drain-read-buffer socket)))
           (ssl-tcp-stream (when passed-in-stream-p
                             (make-instance 'async-io-stream :socket ssl-socket))))
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
                              (cffi:callback as::tcp-read-cb)
                              (cffi:callback as::tcp-write-cb)
                              (cffi:callback ssl-event-cb)
                              data-pointer))

      ;; match up the socket timeouts
      (set-socket-timeouts ssl-socket 1 nil)
      
      ;; make sure the new socket is enabled
      (le:bufferevent-enable ssl-bev (logior le:+ev-read+ le:+ev-write+))

      ;; depending on what object we passed in (socket/stream) return the SSL
      ;; equivalent
      (if passed-in-stream-p
          ssl-tcp-stream
          ssl-socket))))

