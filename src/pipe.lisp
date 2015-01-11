(in-package :cl-async)

(defclass pipe (streamish) ())

(defclass pipe-mixin () ())
(defclass pipe (pipe-mixin socket) ())
(defclass pipe-server (pipe-mixin socket-server) ())

(defmethod server-socket-class ((server pipe-server)) 'tcp-socket)

(defmethod make-socket-handle ((socket-or-server pipe-mixin))
  (let ((s (uv:alloc-handle :named-pipe)))
    ;; TBD: support passing descriptors via pipe (ipc != 0)
    (uv:uv-pipe-init (event-base-c *event-base*) s 0)
    s))

(defmethod socket-server-bind ((server pipe-server) address fd)
  (if fd
      (uv:uv-pipe-open (socket-server-c server) fd)
      (uv:uv-pipe-bind (socket-server-c server) address)))

(defun %pipe-connect (socket/stream name)
  "Connect a pipe initialized with init-client-socket."
  (let* ((socket (if (subtypep (type-of socket/stream) 'async-stream)
                     (streamish socket/stream)
                     socket/stream))
         (uvstream (socket-c socket)))
    ;; track the connection
    (incf (event-base-num-connections-out *event-base*))
    (let ((req (uv:alloc-req :connect)))
      ;; make sure we can grab the original uvstream from the req
      (attach-data-to-pointer req uvstream)
      (uv:uv-pipe-connect req uvstream name (cffi:callback socket-connect-cb))
      socket/stream)))

(defun pipe-connect (name read-cb
                     &key data stream event-cb connect-cb write-cb
                       (read-timeout -1)
                       (write-timeout -1)
                       (dont-drain-read-buffer nil dont-drain-read-buffer-supplied-p))
  "Open a pipe connection asynchronously. Optionally send data out once connected
   via the :data keyword (can be a string or byte array)."
  (check-type data (or null (simple-array octet (*)) string))
  (let ((socket/stream (apply #'init-client-socket
                              'pipe
                              (append (list :read-cb read-cb
                                            :event-cb event-cb
                                            :data data
                                            :stream stream
                                            :connect-cb connect-cb
                                            :write-cb write-cb
                                            :read-timeout read-timeout
                                            :write-timeout write-timeout)
                                      (when dont-drain-read-buffer-supplied-p
                                        (list :dont-drain-read-buffer dont-drain-read-buffer))))))
    (%pipe-connect socket/stream (namestring name))))

(defun pipe-server (name read-cb &key event-cb connect-cb backlog stream fd)
  "Start a pipe listener on the current event loop. Returns a tcp-server class
   which can be closed with close-tcp-server"
  (socket-server 'pipe-server
                 (namestring name) read-cb
                 :event-cb event-cb
                 :connect-cb connect-cb
                 :backlog backlog
                 :stream stream
                 :fd fd))

(defmethod handle-cleanup ((handle-type (eql :named-pipe)) handle)
  (handle-cleanup :async-socket handle))

;; TBD: export socket/streamish (...closed-p etc. stuff, too)
;; TBD: stream-socket replacement!!!
;; TBD: export socket conditions
;; TBD: refactor test
;; TBD: use mkdtemp for pipe tests (available via libuv)
;; TBD: loop-exit-walk-cb (use generics, separate case for plain streams)
;; TBD: to issues: uv_tcp_connect() errors not checked in tcp
;; TBD: utf-8 streams
