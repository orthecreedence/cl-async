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
           #:enable-threading-support
           ;#:enable-debug-mode
           #:start-event-loop
           #:event-loop-exit

           #:timer

           #:connection-dns-error
           #:dns-lookup

           #:tcp-send
           #:tcp-server
           #:write-socket-data
           #:close-socket
           #:set-socket-timeouts
           #:read-socket-data

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
