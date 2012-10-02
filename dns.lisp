(in-package :cl-async)

(define-condition connection-dns-error (connection-error) ()
  (:report (lambda (c s) (format s "Connection DNS error: ~a, ~a~%" (conn-fd c) (conn-errmsg c))))
  (:documentation "Passed to a failure callback when a DNS error occurs on a connection."))

(defun free-dns-base (dns-base)
  "Free a dns base."
  (when dns-base
    (unless (cffi:null-pointer-p dns-base)
      (le:evdns-base-free dns-base 0)
      (clear-object-attachments dns-base))))

(cffi:defcallback dns-cb :void ((errcode :int) (addrinfo :pointer) (dns-base :pointer))
  "Callback for DNS lookups."
  (unwind-protect
    (let* ((callbacks (get-callbacks dns-base))
           (resolve-cb (getf callbacks :resolve-cb))
           (fail-cb (getf callbacks :fail-cb)))
      (if (not (zerop errcode))
          ;; DNS call failed, get error
          (funcall fail-cb (make-instance 'connection-dns-error :code errcode :msg (le:evutil-gai-strerror errcode)))
          ;; success, pull out address
          (let ((family (le-a:evutil-addrinfo-ai-family addrinfo))
                (addr nil))
            (cond
              ((eq family le:+af-inet+)
               (cffi:with-foreign-object (buf :unsigned-char 128)
                 (let* ((sin-addr (cffi:foreign-slot-pointer (le-a:evutil-addrinfo-ai-addr addrinfo) (le::cffi-type le::sockaddr-in) 'le::sin-addr)))
                   (setf addr (le:evutil-inet-ntop family sin-addr buf 128)))))
              (t
                ;; probably ipv6, not supported ATM
                ))
            (if addr
                (funcall resolve-cb addr family)
                (funcall fail-cb (make-instance 'connection-dns-error :code -1 :msg (format nil "Error pulling out address from family: ~a" family))))
            (unless (cffi:null-pointer-p addrinfo)
              (le:evutil-freeaddrinfo addrinfo)))))
    (free-dns-base dns-base)))

(defun dns-lookup (host resolve-cb fail-cb)
  "Asynchronously lookup a DNS address."
  (check-event-loop-running)
  (let ((dns-base (le:evdns-base-new *event-base* 1)))
    (make-foreign-type (hints (le::cffi-type le::evutil-addrinfo) :initial #x0)
                       (('le::ai-family le:+af-inet+)  ;; only want ipv4 for now
                        ('le::ai-flags le:+evutil-ai-canonname+)
                        ('le::ai-socktype le:+sock-stream+)
                        ('le::ai-protocol le:+ipproto-tcp+))
      (save-callbacks dns-base (list :resolve-cb resolve-cb :fail-cb fail-cb))
      (let ((dns-req (le:evdns-getaddrinfo dns-base host (cffi:null-pointer) hints (cffi:callback dns-cb) dns-base)))
        t))))

