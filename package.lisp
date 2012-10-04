(defpackage :cl-async
  (:use :cl)
  (:export #:*catch-application-errors*
           #:*default-event-handler*
           #:connection-info
           #:conn-fd
           #:connection-error
           #:conn-errcode
           #:conn-errmsg
           #:connection-eof
           #:connection-timeout
           #:connection-refused
           ;#:enable-threading-support
           ;#:enable-debug-mode
           #:stats
           #:start-event-loop
           #:event-loop-exit

           #:delay
           #:timer

           #:connection-dns-error
           #:dns-lookup

           #:close-socket
           #:write-socket-data
           ;#:read-socket-data
           #:set-socket-timeouts
           #:tcp-send
           #:tcp-server

           #:http-client
           #:http-response
           #:http-server
           #:http-request
           #:http-request-c
           #:http-request-method
           #:http-request-uri
           #:http-request-resource
           #:http-request-querystring
           #:http-request-headers
           #:http-request-body

           #:setup-signal-handler)
  (:nicknames :as))
