(in-package :cl-async)

(define-condition dns-error (event-error) ()
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
                ;; hmm, didn't get an address. either came back as ipv6 or
                ;; there was some horrible, horrible error.
                (run-event-cb event-cb
                              (make-instance 'dns-error
                                             :code -1
                                             :msg err))))
          ;; error, signal
          (run-event-cb 'event-handler status event-cb))
      (uv:free-req req)
      (free-pointer-data req :preserve-pointer t)
      (uv:uv-freeaddrinfo addrinfo))))

(defun dns-lookup (host resolve-cb &key event-cb (family +af-inet+))
  "Asynchronously lookup a DNS address. Note that if an IP address is passed,
   the lookup happens synchronously. If a lookup is synchronous (and instant)
   this returns T, otherwise nil (lookup happening in background). Either way
   the resolve-cb is called with the lookup info (so always assume this is
   async)."
  (check-event-loop-running)
  (assert (member family (list +af-inet+ +af-inet6+ +af-unspec+)))
  (let ((lookup-c (uv:alloc-req :getaddrinfo))
        (loop-c (event-base-c *event-base*)))
    (with-foreign-object* (hints uv:addrinfo)
                          ((uv-a:addrinfo-ai-family family)
                           (uv-a:addrinfo-ai-flags 0) ;#x2000 AI_CANONNAME
                           (uv-a:addrinfo-ai-socktype uv:+sock-stream+)
                           (uv-a:addrinfo-ai-protocol uv:+ipproto-tcp+))
      (save-callbacks lookup-c (list :resolve-cb resolve-cb
                                     :event-cb event-cb))
      (let ((res (uv:uv-getaddrinfo loop-c lookup-c (cffi:callback dns-cb) host (cffi:null-pointer) hints)))
        (if (< res 0)
            (event-handler res event-cb :throw t)
            t)))))

(define-c-callback reverse-dns-cb :void ((req :pointer) (status :int) (hostname :string) (service :string))
  "Callback for reverse DNS lookups."
  (let* ((callbacks (get-callbacks req))
         (resolve-cb (getf callbacks :resolve-cb))
         (event-cb (getf callbacks :event-cb)))
    (catch-app-errors event-cb
      (if (zerop status)
          (funcall resolve-cb hostname service)
          (run-event-cb 'event-handler status event-cb))
      (uv:free-req req)
      (free-pointer-data req :preserve-pointer t))))

(defun reverse-dns-lookup (ip resolve-cb &key event-cb)
  "Perform reverse DNS lookup on IP specifier as string.  Call RESOLVE-CB with
   resolved HOST and SERVICE as strings.  The callback is called once with one
   host, even if multiple hosts match the query."
  (check-event-loop-running)
  (let ((lookup-c (uv:alloc-req :getnameinfo))
        (loop-c (event-base-c *event-base*)))
    (flet ((lookup (addr)
             (save-callbacks lookup-c (list :resolve-cb resolve-cb :event-cb event-cb))
             (let ((res (uv:uv-getnameinfo loop-c lookup-c (cffi:callback reverse-dns-cb) addr 0)))
               (if (< res 0)
                   (event-handler res event-cb :throw t)
                   t))))
      (if (and ip (find #\: ip))
          (with-foreign-object* (addr uv:sockaddr-in6)
                                ((uv-a:sockaddr-in-sin-family +af-inet6+))
            (uv:uv-inet-pton +af-inet6+ ip (cffi:foreign-slot-pointer addr '(:struct uv:sockaddr-in6) 'uv:sin6-addr))
            (lookup addr))
          (with-foreign-object* (addr uv:sockaddr-in)
                                ((uv-a:sockaddr-in-sin-family +af-inet+))
            (uv:uv-inet-pton +af-inet+ ip (cffi:foreign-slot-pointer addr '(:struct uv:sockaddr-in) 'uv:sin-addr))
            (lookup addr))))))
