(defpackage :cl-async-util
  (:use :cl :cl-async-base)
  (:export #:octet
           #:octet-vector
           #:bytes-or-string
           #:callback

           #:bytes
           #:make-buffer
           #:buffer-output
           #:write-to-buffer

           #:do-chunk-data

           #:+af-inet+
           #:+af-inet6+
           #:+af-unspec+
           #:+af-unix+

           #:+sockaddr-size+
           #:+sockaddr6-size+
           #:+addrinfo-size+

           #:*passthrough-errors*
           #:passthrough-error-p
           #:handle-error
           #:abort-callback
           #:exit-event-loop
           #:continue-event-loop
           #:call-with-callback-restarts
           #:catch-app-errors
           #:run-event-cb

           #:define-c-callback

           #:with-foreign-object*

           #:with-lock

           #:make-pointer-eql-able
           #:create-data-pointer
           #:save-callbacks
           #:get-callbacks
           #:clear-callbacks
           #:attach-data-to-pointer
           #:deref-data-from-pointer
           #:clear-pointer-data
           #:free-pointer-data

           #:with-struct-timeval
           #:split-usec-time

           #:append-array

           #:*ipv4-scanner*
           #:*ipv6-scanner*

           #:error-str

           #:ipv4-address-p
           #:ipv6-address-p
           #:ip-address-p
           #:ip-str-to-sockaddr
           #:with-ip-to-sockaddr
           #:addrinfo-to-string

           #:set-socket-nonblocking
           #:fd-connected-p

           #:define-condition-alias))

