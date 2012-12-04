(defpackage :cl-async-ssl
  (:use :cl :cl-async :cl-async-util)
  (:nicknames :as-ssl)
  (:export #:wrap-socket-in-ssl))
(in-package :cl-async-ssl)

(define-condition tcp-ssl-error (tcp-error) ()
  (:report (lambda (c s) (format s "SSL connection error: ~a: ~a" (conn-errcode c) (conn-errmsg c))))
  (:documentation "Describes a general SSL connection error."))

(defclass ssl-socket (socket) ())

(defmethod close-socket ((socket ssl-socket))
  (call-next-method))

(cffi:defcallback ssl-event-cb :void ((bev :pointer) (events :short) (data-pointer :pointer))
  "Called whenever anything happens on a TCP socket. Ties into the anonymous
   callback system to track failures/disconnects."
  (let* ((bev-data (deref-data-from-pointer bev))
         (socket (getf bev-data :socket))
         (event-cb (getf (get-callbacks data-pointer) :event-cb)))
    (catch-app-errors event-cb
      (let ((errcode (le-ssl:bufferevent-get-openssl-error bev)))
        (unless (zerop errcode)
          (setf events le:+bev-event-error+)
          (cffi:with-foreign-object (buf :unsigned-char 256)
            (cl+ssl::err-error-string errcode buf)
            (let ((str (cffi:foreign-string-to-lisp buf)))
              (funcall event-cb (make-instance 'tcp-ssl-error :code errcode :msg str)))
            (close-socket socket)))
        (cffi:foreign-funcall-pointer
          (cffi:callback as::tcp-event-cb) ()
          :pointer bev :short events :pointer data-pointer)))))

(defun wrap-socket-in-ssl (socket/stream &key certificate key password (method 'cl+ssl::ssl-v23-client-method) close-cb)
  "Wraps a cl-async socket/stream in the SSL protocol using libevent's SSL
   capabilities. Does some trickery when swapping out the event function for the
   new socket, but the original event-cb will still be called."
  (declare (ignore close-cb))
  (cl+ssl:ensure-initialized :method method)  ; make sure SSL is ready to go

  (let* ((global-ctx cl+ssl::*ssl-global-context*)
         (data-pointer (create-data-pointer))
         (passed-in-stream (subtypep (type-of socket/stream) 'async-stream))
         (socket (if passed-in-stream
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
                      (cffi:foreign-enum-value 'le-ssl:bufferevent-ssl-state ':bufferevent-ssl-connecting)
                      (cffi:foreign-enum-value 'le:bufferevent-options ':+bev-opt-close-on-free+)))
           ;; make an SSL socket that matches the options of the original socket
           (ssl-socket (make-instance 'ssl-socket
                                      :c ssl-bev
                                      :drain-read-buffer (as::socket-drain-read-buffer socket)))
           (ssl-tcp-stream (when passed-in-stream (make-instance 'async-io-stream :socket ssl-socket))))
      ;; get rid of any references to our original data pointers. this should free
      ;; up some potential memory issues associated with replacing the pointers
      ;; with the new sockets/streams/callbacks/etc below
      (let ((original-data-pointer (getf (deref-data-from-pointer bufferevent-orig) :data-pointer)))
        ;; make sure if there's any data/callbacks on the original data pointer,
        ;; we forward it to the new data pointer
        (save-callbacks data-pointer (get-callbacks original-data-pointer))
        (attach-data-to-pointer data-pointer (deref-data-from-pointer original-data-pointer))
        (free-pointer-data original-data-pointer)
        (free-pointer-data bufferevent-orig :preserve-pointer t))

      ;; allow our callbacks to deref data directly from the bev
      (attach-data-to-pointer ssl-bev (list :data-pointer data-pointer
                                            :socket ssl-socket
                                            :stream ssl-tcp-stream))
      ;; setup the callbacks for our ssl bufferevent. notice we hijack the event
      ;; callback (since we need some special processing). it will call the
      ;; normal tcp-event-cb when it's done processing.
      (le:bufferevent-setcb ssl-bev
                            (cffi:callback as::tcp-read-cb)
                            (cffi:callback as::tcp-write-cb)
                            (cffi:callback ssl-event-cb)
                            data-pointer)
      (le:bufferevent-enable ssl-bev (logior le:+ev-read+ le:+ev-write+))

      ;; match up the socket timeouts
      (set-socket-timeouts ssl-socket 1 nil)
      
      ;; depending on what object we passed in (socket/stream) return the SSL
      ;; equivalent
      (if passed-in-stream
          ssl-tcp-stream
          ssl-socket))))

