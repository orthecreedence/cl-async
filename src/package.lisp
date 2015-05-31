(defpackage :cl-async
  (:use :cl :cl-async-base :cl-async-util :trivial-gray-streams)
  (:nicknames :as)
  (:shadow cl-async-util:exit-event-loop)
  (:export #:octet
           #:octet-vector

           #:handle-error

           #:bytes

           #:+af-inet+
           #:+af-inet6+
           #:+af-unspec+
           #:+af-unix+

           #:*buffer-writes*
           #:*buffer-size*

           ;; common conditions/accessors
           #:event-info
           #:event-error
           #:event-errcode
           #:event-errmsg
           ;; common functions
           #:stats
           #:dump-event-loop-status
           #:add-event-loop-exit-callback
           #:ref
           #:unref
           #:start-event-loop
           #:with-event-loop
           #:exit-event-loop

           ;; event functions
           #:event-freed
           #:event
           #:event-c
           #:event-freed-p
           #:free-event
           #:remove-event
           #:add-event
           #:delay
           #:with-delay
           #:interval
           #:with-interval
           #:remove-interval
           #:make-event
           #:watch-fd
           #:fd-add

           ;; notifier exports
           #:notifier
           #:notifier-freed-p
           #:free-notifier
           #:make-notifier
           #:trigger-notifier

           ;; signal numbers
           #:+sighup+
           #:+sigint+
           #:+sigquit+
           #:+sigill+
           #:+sigtrap+
           #:+sigabrt+
           #:+sigemt+
           #:+sigfpe+
           #:+sigkill+
           #:+sigbus+
           #:+sigsegv+
           #:+sigsys+
           #:+sigpipe+
           #:+sigalrm+
           #:+sigterm+
           #:+sigurg+
           #:+sigstop+
           #:+sigtstp+
           #:+sigcont+
           #:+sigchld+
           #:+sigttin+
           #:+sigttou+
           #:+sigio+
           #:+sigxcpu+
           #:+sigxfsz+
           #:+sigvtalrm+
           #:+sigprof+
           #:+sigwinch+
           #:+siginfo+
           #:+sigusr1+
           #:+sigusr2+
           ;; signal handling functions
           #:signal-handler
           #:free-signal-handler
           #:clear-signal-handlers

           ;; dns conditions
           #:dns-error
           ;; dns functions
           #:dns-lookup
           #:reverse-dns-lookup

           ;; streamish/socket/tcp/pipe conditions/accessors
           #:streamish
           #:close-streamish
           #:streamish-info
           #:streamish-error
           #:streamish-eof
           #:streamish-broken-pipe
           #:streamish-canceled
           #:streamish-closed
           #:check-streamish-open
           #:streamish-closed-p
           #:close-streamish
           #:streamish-write

           #:socket-info
           #:socket-socket
           #:socket-error
           #:socket-eof
           #:socket-reset
           #:socket-timeout
           #:socket-refused
           #:socket-aborted
           #:socket-address-in-use
           #:socket-accept-error
           #:socket-accept-error-listener
           #:socket-accept-error-tcp-server
           #:tcp-info
           #:tcp-socket
           #:tcp-error
           #:tcp-eof
           #:tcp-reset
           #:tcp-timeout
           #:tcp-refused
           #:tcp-accept-error
           #:socket-closed
           #:tcp-server-bind-error
           ;; socket class/accessors
           #:socket
           #:socket-c
           #:socket-data
           ;; tcp functions
           #:socket-closed-p
           #:close-socket
           #:close-socket-server
           #:close-tcp-server
           #:write-socket-data
           #:set-socket-timeouts
           #:enable-socket
           #:disable-socket
           #:init-tcp-socket
           #:connect-tcp-socket
           #:tcp-connect
           #:tcp-server

           #:pipe-connect
           #:pipe-server
           #:pipe-not-found

           ;; tcp stream
           #:async-stream
           #:stream-socket
           #:async-input-stream
           #:async-output-stream
           #:async-io-stream

           ;; idler
           #:idler
           #:idler-freed-p
           #:free-idler
           #:idle

           #:poller
           #:poller-freed-p
           #:free-poller
           #:poll

           #:mkdtemp
           #:filesystem-error
           #:filesystem-enoent
           #:filesystem-eacces
           #:filesystem-eperm

           #:spawn
           #:process-input
           #:process-output
           #:process-error-output
           #:process-kill

           #:fs-watch
           #:fs-unwatch
           #:fs-monitor-close))
