(in-package :cl-async)

(define-condition dns-error (event-error) ()
  (:report (lambda (c s) (format s "DNS error: ~a, ~a" (event-errcode c) (event-errmsg c))))
  (:documentation "Passed to a failure callback when a DNS error occurs on a connection."))

(defvar *windows-local-hosts* nil
  "Holds all hosts entries from the Windows hosts file.")

(defparameter *scanner-parse-hosts-entries*
  (cl-ppcre:create-scanner "[ \\s\\t\\r\\n]+" :case-insensitive-mode t)
  "A regex for matching hosts entries in a windows hosts file.")

(defun populate-windows-local-hosts (&key force)
  "Grab and store all entries from the windows hosts file. If we already have
   the data, skip (unless :force t is passed)."
  (when (or (null *windows-local-hosts*) force)
    (setf *windows-local-hosts* (make-hash-table :test #'equal))
    ;; TODO find a way to get windows dir from %WINDOWS% instead of using a
    ;; hardcoded path. Mayb there's a way to read ENV vars?
    (let ((hostsfile "c:/windows/system32/drivers/etc/hosts"))
      (when (probe-file hostsfile)
        (with-open-file (s hostsfile :direction :input)
          (loop for line = (read-line s nil :eof)
                until (eq line :eof) do
            (unless (member (aref line 0)
                            '(#\# #\return #\newline #\space #\tab))
              (let* ((entries (cl-ppcre:split *scanner-parse-hosts-entries* line))
                     (ip (car entries)))
                (when ip
                  (dolist (host (cdr entries))
                    (setf (gethash host *windows-local-hosts*) ip))))))))))
  *windows-local-hosts*)

(defun get-dns-base ()
  "Grabs the current DNS base (or instantiates if it doesn't exist) and also
   tracks how many open DNS base queries there are."
  (prog1 (if (event-base-dns-base *event-base*)
             (event-base-dns-base *event-base*)
             (let ((dns-base (le:evdns-base-new (event-base-c *event-base*) 1))
                   (dns-windows (cffi:foreign-symbol-pointer "evdns_base_config_windows_nameservers")))
               (if dns-windows
                   ;; we're on windows, so load entries from registry
                   (progn
                     (populate-windows-local-hosts)
                     (le:evdns-base-search-clear dns-base)
                     (cffi:foreign-funcall-pointer dns-windows () :pointer dns-base :int))
                   ;; on *nix, so load from resolv.conf (if available)
                   (when (probe-file #P"/etc/resolv.conf")
                     (le:evdns-base-resolv-conf-parse
                       dns-base
                       le:+dns-options-all+
                       "/etc/resolv.conf")))
               (setf (event-base-dns-ref-count *event-base*) 0
                     (event-base-dns-base *event-base*) dns-base)))
    (incf (event-base-dns-ref-count *event-base*))))

(defun free-dns-base (dns-base)
  "Free a dns base."
  (when dns-base
    (unless (cffi:null-pointer-p dns-base)
      (free-pointer-data dns-base :preserve-pointer t)
      (le:evdns-base-free dns-base 0))))

(defun release-dns-base ()
  "Decrements the DNS base reference counter. If there are no more references,
   frees the DNS base."
  (decf (event-base-dns-ref-count *event-base*))
  (when (<= (event-base-dns-ref-count *event-base*) 0)
    (free-dns-base (event-base-dns-base *event-base*))
    (setf (event-base-dns-ref-count *event-base*) 0)
    (setf (event-base-dns-base *event-base*) nil)))

(define-c-callback dns-log-cb :void ((is-warning :int) (msg :string))
  "Callback for DNS logging."
  (let* ((callbacks (get-callbacks :dns-callbacks))
         (event-cb (getf callbacks :event-cb))
         (log-cb (getf callbacks :log-cb)))
    (catch-app-errors event-cb
      (when log-cb
        (funcall log-cb is-warning msg)))))

(define-c-callback dns-cb :void ((errcode :int) (addrinfo :pointer) (data-pointer :pointer))
  "Callback for DNS lookups."
  (let* ((callbacks (get-callbacks data-pointer))
         (resolve-cb (getf callbacks :resolve-cb))
         (event-cb (getf callbacks :event-cb)))
    (unwind-protect
      (catch-app-errors event-cb
        (if (not (zerop errcode))
            ;; DNS call failed, get error
            (run-event-cb event-cb (make-instance 'dns-error :code errcode :msg (le:evutil-gai-strerror errcode)))

            ;; success, pull out address
            (let ((family (cffi:foreign-slot-value addrinfo *addrinfo* 'le::ai-family))
                  (addr nil)
                  (err nil))

              ;(dotimes (i (cffi:foreign-type-size *addrinfo*))
              ;  (format t "byte ~a: ~a~%" i (cffi:mem-aref addrinfo :unsigned-char i)))
              ;(let ((addr-pt (cffi:pointer-address addrinfo))
              ;      (addr-pt-off (cffi:pointer-address (cffi:foreign-slot-pointer addrinfo *addrinfo* 'le::ai-addr))))
              ;  (format t "addr: ~a~%addr: ~a~%diff: ~a~%" addr-pt addr-pt-off (- addr-pt-off addr-pt)))

              (cffi:with-foreign-object (buf :unsigned-char 128)
                ;; note here, we use the OS-dependent addrinfo-ai-addr macro
                ;; defined in util.lisp
                (let* ((ai-addr (addrinfo-ai-addr addrinfo)))
                  (if (cffi:null-pointer-p ai-addr)
                      (setf err "the addrinfo->ai_addr object was null (stinks of a memory alignment issue)")
                      (cond
                        ((eq family +af-inet+)
                         (let ((sin-addr (cffi:foreign-slot-pointer ai-addr (le::cffi-type le::sockaddr-in) 'le::sin-addr)))
                           ;(dotimes (i +sockaddr-size+)
                             ;(format t "byte ~a: ~a~%" i (cffi:mem-aref ai-addr :unsigned-char i)))
                           (setf addr (le:evutil-inet-ntop family sin-addr buf 128))))
                        ((eq family +af-inet6+)
                         (let ((sin6-addr (cffi:foreign-slot-pointer ai-addr (le::cffi-type le::sockaddr-in-6) 'le::sin-6-addr-0)))
                           (setf addr (le:evutil-inet-ntop family sin6-addr buf 128))))
                        (t
                          (setf err (format nil "unsupported DNS family: ~a" family)))))))
              (if (and addr (not err))
                  ;; got an address, call resolve-cb
                  (funcall resolve-cb addr family)
                  ;; hmm, didn't get an address. either cam back as ipv6 or 
                  ;; there was some horrible, horrible error.
                  (run-event-cb event-cb
                                (make-instance 'dns-error
                                               :code -1
                                               :msg err)))

              ;; clean up
              (unless (cffi:null-pointer-p addrinfo)
                (le:evutil-freeaddrinfo addrinfo)))))
      (free-pointer-data data-pointer)
      (release-dns-base))))

(defun start-dns-logging (log-cb &key event-cb)
  "Send all DNS messages to log-cb. Optionally takes an event-cb to catch errors
   that occur during log processing."
  (save-callbacks :dns-callbacks (list :log-cb log-cb
                                       :event-cb event-cb))
  (le:evdns-set-log-fn (cffi:callback dns-log-cb)))

(defun stop-dns-logging ()
  "Stop logging DNS messages through the log-cb passed to start-dns-logging."
  (free-pointer-data :dns-callbacks :preserve-pointer t)
  (le:evdns-set-log-fn (cffi:null-pointer)))

(defun dns-lookup (host resolve-cb event-cb &key (family *default-lookup-type*))
  "Asynchronously lookup a DNS address. Note that if an IP address is passed,
   the lookup happens synchronously. If a lookup is synchronous (and instant)
   this returns T, otherwise nil (lookup happening in background). Either way
   the resolve-cb is called with the lookup info (so always assume this is
   async)."
  (check-event-loop-running)
  (assert (member family (list +af-inet+ +af-inet6+ +af-unspec+)))
  (let ((local-entry (when (hash-table-p *windows-local-hosts*)
                       (gethash host *windows-local-hosts*))))
    (if (and local-entry
             (cond ((= family +af-unspec+) t)
                   ((= family +af-inet+) (ipv4-address-p local-entry))
                   ((= family +af-inet6+) (ipv6-address-p local-entry))))
        ;; AHHHHHAHAHAHA!!!!!!!!!! windows local host: fire callback directly
        (funcall resolve-cb local-entry family)

        ;; do a real lookup
        (let ((data-pointer (create-data-pointer))
              (dns-base (get-dns-base)))
          (make-foreign-type (hints *addrinfo* :initial #x0 :type-size +addrinfo-size+)
                             (('le::ai-family family)  ;; only want ipv4 for now
                              ('le::ai-flags le:+evutil-ai-canonname+)
                              ('le::ai-socktype le:+sock-stream+)
                              ('le::ai-protocol le:+ipproto-tcp+))
            (save-callbacks data-pointer (list :resolve-cb resolve-cb :event-cb event-cb))
            (attach-data-to-pointer data-pointer dns-base)
            (let ((dns-req (le:evdns-getaddrinfo dns-base host (cffi:null-pointer) hints (cffi:callback dns-cb) data-pointer)))
              (cffi:null-pointer-p dns-req)))))))

