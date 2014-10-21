(in-package :cl-async)

(define-condition dns-error (event-error) ()
  (:report (lambda (c s) (format s "DNS error: ~a, ~a" (event-errcode c) (event-errmsg c))))
  (:documentation "Passed to a failure callback when a DNS error occurs on a connection."))

(define-c-callback dns-cb :void ((req :pointer) (status :int) (addrinfo :pointer))
  "Callback for DNS lookups."
  (let* ((callbacks (get-callbacks req))
         (resolve-cb (getf callbacks :resolve-cb))
         (event-cb (getf callbacks :event-cb)))
    (catch-app-errors event-cb
      (if (zerop status)
          ;; success, pull out address
          (multiple-value-bind (addr family err)
              (addrinfo-to-string addrinfo)
            (if (and addr (not err))
                ;; got an address, call resolve-cb
                (funcall resolve-cb addr family)
                ;; hmm, didn't get an address. either cam back as ipv6 or 
                ;; there was some horrible, horrible error.
                (run-event-cb event-cb
                              (make-instance 'dns-error
                                             :code -1
                                             :msg err))))
          (event-handler status event-cb))
      (uv:free-req req)
      (uv:uv-freeaddrinfo addrinfo))))
      
(defun dns-lookup (host resolve-cb event-cb &key (family *default-lookup-type*))
  "Asynchronously lookup a DNS address. Note that if an IP address is passed,
   the lookup happens synchronously. If a lookup is synchronous (and instant)
   this returns T, otherwise nil (lookup happening in background). Either way
   the resolve-cb is called with the lookup info (so always assume this is
   async)."
  (check-event-loop-running)
  (assert (member family (list +af-inet+ +af-inet6+ +af-unspec+)))
  (let ((lookup-c (uv:alloc-req :getaddrinfo))
        (loop-c (event-base-c *event-base*)))
    (make-foreign-type (hints uv:addrinfo :initial #x0)
                       (('uv::ai-family family)
                        ('uv::ai-flags 0)    ;#x2000 AI_CANONNAME
                        ('uv::ai-socktype uv:+sock-stream+)
                        ('uv::ai-protocol uv:+ipproto-tcp+))
      (save-callbacks lookup-c (list :resolve-cb resolve-cb
                                     :event-cb event-cb))
      (let ((res (uv:uv-getaddrinfo loop-c lookup-c (cffi:callback dns-cb) host (cffi:null-pointer) hints)))
        (if (< res 0)
            (event-handler res event-cb)
            t)))))

