(defpackage :cl-async
  (:use :cl)
  (:export #:*catch-application-errors*
           #:*default-event-handler*
           ;; common conditions/accessors
           #:connection-info
           #:connection-error
           #:conn-errcode
           #:conn-errmsg
           ;; common functions
           #:stats
           #:start-event-loop
           #:event-loop-exit

           ;; timer functions
           #:delay
           #:timer

           ;; signal handling functions
           #:signal-handler
           #:free-signal-handler
           #:clear-signal-handler

           ;; dns conditions
           #:dns-error
           ;; dns functions
           #:dns-lookup

           ;; tcp conditions/accessors
           #:tcp-info
           #:tcp-socket
           #:tcp-error
           #:tcp-eof
           #:tcp-timeout
           #:tcp-refused
           #:socket-closed
           ;; tcp functions
           #:close-socket
           #:close-tcp-server
           #:write-socket-data
           #:set-socket-timeouts
           #:enable-socket
           #:disable-socket
           #:tcp-send
           #:tcp-server

           ;; http conditions/accessors
           #:http-info
           #:http-error
           #:http-timeout
           #:http-refused
           ;;http functions
           #:http-client
           #:http-response
           #:http-server
           #:close-http-server
           ;; http classes/accessors
           #:http-request
           #:http-request-c
           #:http-request-method
           #:http-request-uri
           #:http-request-resource
           #:http-request-querystring
           #:http-request-headers
           #:http-request-body)
  (:nicknames :as))
