(defpackage :cl-async-ssl
  (:use :cl :cl-async)
  (:export #:tcp-send-ssl))
(in-package :cl-async-ssl)

(use-package :cl-async)

(defvar *ssl-init-p* nil)

(define-condition tcp-ssl-error (as:tcp-error) ()
  (:report (lambda (c s) (format s "SSL connection error: ~a: ~a" (conn-errcode c) (conn-errmsg c))))
  (:documentation "Describes a general SSL connection error."))

(cffi:defcallback ssl-event-cb :void ((bev :pointer) (events :short) (data-pointer :pointer))
  "Called whenever anything happens on a TCP socket. Ties into the anonymous
   callback system to track failures/disconnects."
  (let* ((event nil)
         (dns-base (deref-data-from-pointer data-pointer))
         (bev-data (deref-data-from-pointer bev))
         (socket (getf bev-data :socket))
         (event-cb (getf (get-callbacks data-pointer) :event-cb)))
    (catch-app-errors event-cb
      (let ((errcode (le-ssl:bufferevent-get-openssl-error bev)))
        (unless (zerop errcode)
          (setf events le:+bev-event-error+)
          (cffi:with-foreign-object (buf :unsigned-char 256)
            (cl+ssl::err-error-string errcode buf)
            (let ((str (cffi:foreign-string-to-lisp buf)))
              (funcall event-cb (make-instance 'tcp-ssl-error :code errcode :msg str)))))
        (cffi:foreign-funcall-pointer
          (cffi:callback as::tcp-event-cb)
          ()
          :pointer bev :short events :pointer data-pointer)))))

(defun tcp-send-ssl (host port data read-cb event-cb &key write-cb (read-timeout -1) (write-timeout -1) (dont-drain-read-buffer nil dont-drain-read-buffer-supplied-p) stream)
  "Wrapper around cl-async's tcp-send that provides SSL security around an async
   socket."
  ;; load SSL if we have not
  (unless *ssl-init-p*
    (cl+ssl::ssl-load-error-strings)
    (cl+ssl::ssl-library-init)
    (setf *ssl-init-p* t))

  ;; wrap the socket in a libevent SSL wrapper
  (let* ((data-pointer (as::create-data-pointer))
         (socket/stream (apply #'tcp-send
                               (append (list
                                         host port nil nil nil
                                         :write-cb nil
                                         ;host port data read-cb event-cb
                                         ;:write-cb write-cb
                                         :read-timeout read-timeout
                                         :write-timeout write-timeout)
                                       (when dont-drain-read-buffer-supplied-p
                                         (list :dont-drain-read-buffer dont-drain-read-buffer))
                                       :stream stream)))
         (socket (if (subtypep (type-of socket/stream) 'as:async-stream)
                     (stream-socket stream/socket)
                     stream/socket))
         (bufferevent (as::socket-c socket))
         (ssl-ctx (cl+ssl::ssl-ctx-new (cl+ssl::ssl-v23-client-method))))
    ;; make sure we init
    (when (cffi:null-pointer-p ssl-ctx)
      (error "Problem initializing SSL."))

    ;; create our SSL socket/stream and make sure it grabs any callbacks/hooks
    ;; it needs to operate properly from the original bufferevent that tcp-send
    ;; created.
    (let* ((ssl-bev (le-ssl:bufferevent-openssl-filter-new
                      as::*event-base*
                      bufferevent  ; pass in the original bev
                      ssl-ctx
                      (cffi:foreign-enum-value 'le-ssl:bufferevent-ssl-state ':+bufferevent-ssl-connecting+)
                      (cffi:foreign-enum-value 'le:bufferevent-options ':+bev-opt-close-on-free+)))
           (ssl-socket (make-instance 'as:socket :c ssl-bev))
           (ssl-tcp-stream (when stream (make-instance 'as:async-io-stream :socket ssl-socket))))
      ;; get rid of any references to our original data pointers. this should free
      ;; up some potential memory issues associated with replacing the pointers
      ;; with the new sockets/streams/callbacks/etc below
      (let ((original-data-pointer (as::deref-data-from-pointer bufferevent)))
        ;; make sure if there's any data on the original data pointer, we
        ;; forward it to the new data pointer
        (as::attach-data-to-pointer data-pointer (as::deref-data-from-pointer original-data-pointer))
        (as::free-pointer-data original-data-pointer)
        (as::free-pointer-data bufferevent :preserve-pointer t))

      (as::save-callbacks data-pointer (list :read-cb read-cb :event-cb event-cb :write-cb write-cb))
      (as::attach-data-to-pointer ssl-bev (list :data-pointer data-pointer
                                                :socket ssl-socket
                                                :stream ssl-tcp-stream))
      ;; setup the callbacks for our ssl bufferevent. notice we hijack the event
      ;; callback (since we need some special processing). it will call the
      ;; normal tcp-event-cb when it's done processing.
      (le:bufferevent-setcb ssl-bev
                            (cffi:callback as::tcp-read-cb)
                            (cffi:callback as::tcp-write-cb)
                            (cffi:callback as::ssl-event-cb)
                            data-pointer)
      ;; update the callbacks and send in our data. wicked.
      (write-socket-data ssl-socket data
        :read-cb read-cb
        :write-cb write-cb
        :event-cb event-cb))))


