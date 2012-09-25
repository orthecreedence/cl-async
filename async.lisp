(defpackage :cl-async
  (:use :cl)
  (:export #:close-socket
           #:set-socket-timeouts
           #:write-socket-data
           #:timer
           #:tcp-async-send
           #:tcp-async-server
           #:start-event-loop
           #:event-loop-exit)
  (:nicknames :as))
(in-package :cl-async)

(defvar *event-base* nil
  "THE event base (libevent) used to process all async operations.")
(defvar *fn-registry* nil
  "Function registry, allows the CFFI callbacks to run anonymous functions.")
(defvar *event-loop-end-functions* nil
  "Functions to call when the event loop closes")

(defmacro make-foreign-type ((var type &key initial) bindings &body body)
  "Convenience macro, makes creation and initialization of CFFI types easier.
   Emphasis on initialization."
  `(cffi:with-foreign-object (,var ,type)
     (when ,initial
       (cffi:foreign-funcall "memset" :pointer ,var :unsigned-char ,initial :unsigned-char (cffi:foreign-type-size ,type)))
     ,@(loop for binding in bindings collect
         `(setf (cffi:foreign-slot-value ,var ,type ,(car binding)) ,(cadr binding)))
     ,@body))

(defun save-callbacks (pointer &key read-cb write-cb fail-cb timer-cb)
  "Save a set of callbacks, keyed by the given object (pointer)."
  (unless *fn-registry*
    (setf *fn-registry* (make-hash-table :test #'eql)))
  (setf (gethash pointer *fn-registry*) (list :read-cb read-cb
                                              :write-cb write-cb
                                              :fail-cb fail-cb
                                              :timer-cb timer-cb)))

(defun get-callbacks (pointer)
  "Get all callbacks for the given object (pointer)."
  (when *fn-registry*
    (prog1 (gethash pointer *fn-registry*))))

(defun clear-callbacks (pointer)
  "Clear out all callbacks for the given object (pointer)."
  (when *fn-registry*
    (remhash pointer *fn-registry*)))

(defun split-usec-time (time-s)
  "Given a second value, ie 3.67, return the number of seconds as the first
   value and the number of usecs for the second value."
  (if (numberp time-s)
      (multiple-value-bind (time-sec time-frac) (floor time-s)
        (values time-sec (floor (* 1000000 time-frac))))
      nil))

(defun close-socket (bev)
  "Free a socket and clear out all associated data."
  (clear-callbacks bev)
  (le::bufferevent-free bev))

(defun free-listener (listener)
  "Free a socket listener and all associated data."
  (clear-callbacks listener)
  (le::evconnlistener-free listener))

(defun set-socket-timeouts (socket read-sec write-sec)
  "Given a pointer to a libevent socket (bufferevent), set the read/write
   timeouts on the bufferevent."
  (multiple-value-bind (read-sec read-usec) (split-usec-time read-sec)
    (multiple-value-bind (write-sec write-usec) (split-usec-time write-sec)
      (make-foreign-type (read-to (le::cffi-type le::timeval))
                         (('le::tv-sec read-sec)
                          ('le::tv-usec read-usec))
        (make-foreign-type (write-to (le::cffi-type le::timeval))
                           (('le::tv-sec write-sec)
                            ('le::tv-usec write-usec))
          (let ((read-to (if (numberp read-sec) read-to (cffi:null-pointer)))
                (write-to (if (numberp write-sec) write-to (cffi:null-pointer))))
          (le::bufferevent-set-timeouts socket read-to write-to)))))))

(defun write-socket-data (socket data)
  "Write data into libevent socket (bufferevent)."
  (let ((data (if (stringp data)
                  (flexi-streams:string-to-octets data :external-format :utf8)
                  data)))
    (cffi:with-foreign-object (data-c :unsigned-char (length data))
      (dotimes (i (length data))
        (setf (cffi:mem-aref data-c :unsigned-char i) (aref data i)))
      (le::evbuffer-add (le::bufferevent-get-output socket) data-c (length data)))))

(defun add-event-loop-exit-callback (fn)
  "Add a function to be run when the event loop exits."
  (push fn *event-loop-end-functions*))

(defun process-event-loop-exit-callbacks ()
  "run and clear out all event loop exit functions."
  (dolist (fn *event-loop-end-functions*)
    (funcall fn))
  (setf *event-loop-end-functions* nil))

(cffi:defcallback timer-cb :void ((fd :pointer) (what :pointer) (data :pointer))
  "Callback used by the async timer system to find and run user-specified
   callbacks on timer events."
  (declare (ignore fd what))
  (let* ((callbacks (get-callbacks data))
         (timer-cb (getf callbacks :timer-cb)))
    (when timer-cb
      (funcall timer-cb))))

(cffi:defcallback tcp-read-cb :void ((bev :pointer) (event-base :pointer))
  "Called whenever a read event happens on a TCP socket. Ties into the anonymous
   callback system to run user-specified anonymous callbacks on read events."
  (declare (ignore event-base))
  (let* ((callbacks (get-callbacks bev))
         (read-cb (getf callbacks :read-cb)))
    (if read-cb
        (let ((bufsize 1024))
          (cffi:with-foreign-object (buffer :unsigned-char bufsize)
            (let ((input (le::bufferevent-get-input bev))
                  (lbuf (make-array bufsize :element-type '(unsigned-byte 8))))
              (loop for n = (le::evbuffer-remove input buffer bufsize)
                    while (< 0 n) do
                (dotimes (i n)
                  (setf (aref lbuf i) (cffi:mem-aref buffer :char i)))
                (funcall read-cb bev (subseq lbuf 0 n))))))
        (le::evbuffer-drain (le::bufferevent-get-input bev) 1024))))
  
(cffi:defcallback tcp-event-cb :void ((bev :pointer) (events :short) (event-base :pointer))
  "Called whenever anything happens on a TCP socket. Ties into the anonymous
   callback system to track failures/disconnects."
  (cond
    ((< 0 (logand events le::+bev-event-connected+))
     (format t " - Connect OK.~%"))
    ((< 0 (logand events (logior le::+bev-event-error+
                                 le::+bev-event-eof+
                                 le::+bev-event-timeout+)))
     (format t " - Closing: ")
     (when (< 0 (logand events le::+bev-event-error+))
       (let ((err (le::bufferevent-socket-get-dns-error bev)))
         (when (not (zerop err))
           (format t "Error: ~a" (le::evutil-gai-strerror err)))))
     (when (< 0 (logand events le::+bev-event-timeout+))
       (format t "Timeout"))
     (when (< 0 (logand events le::+bev-event-eof+))
       (format t "EOF"))
     (format t "~%")
     (close-socket bev)
     (le::event-base-loopexit event-base (cffi:null-pointer))
     (when (< 0 (logand events (logior le::+bev-event-error+
                                       le::+bev-event-timeout+)))
       (let* ((callbacks (get-callbacks bev))
              (fail-cb (getf callbacks :fail-cb))
              (errors nil))
         (when fail-cb
           (dolist (err (list (cons le::+bev-event-error+ :err)
                              (cons le::+bev-event-timeout+ :timeout)))
             (when (logand events (car err))
               (push (cdr err) errors)))
           (funcall fail-cb bev errors)))))))

(cffi:defcallback tcp-accept-cb :void ((listener :pointer) (fd :int) (addr :pointer) (socklen :int) (ctx :pointer))
  (declare (ignore ctx socklen addr))
  (let* ((event-base (le::evconnlistener-get-base listener))
         (bev (le::bufferevent-socket-new event-base
                                          fd
                                          (cffi:foreign-enum-value 'le::bufferevent-options :+bev-opt-close-on-free+)))
         (callbacks (get-callbacks listener)))
    ;(set-socket-timeouts bev 5 5)
    (apply #'save-callbacks `(,bev ,@callbacks))
    (le::bufferevent-setcb bev
                           (cffi:callback tcp-read-cb)
                           (cffi:null-pointer)
                           (cffi:callback tcp-event-cb)
                           (cffi:null-pointer))
    (le::bufferevent-enable bev (logior le::+ev-read+ le::+ev-write+))))

(cffi:defcallback tcp-accept-err-cb :void ((listener :pointer) (ctx :pointer))
  (declare (ignore ctx))
  (let* ((event-base (le::evconnlistener-get-base listener)))
    (format t "There was an error and I don't know how to get the code.~%")
    (le::event-base-loopexit event-base (cffi:null-pointer))))

(defun check-event-loop-running ()
  (unless *event-base*
    (error "Event loop not running. Start with function start-event-loop.")))

(defun timer (time-s callback)
  "Run a function, asynchronously, after the specified amount of seconds. An
   event loop must be running for this to work."
  (check-event-loop-running)
  (multiple-value-bind (time-sec time-usec) (split-usec-time time-s)
    (make-foreign-type (time-c (le::cffi-type le::timeval))
                       (('le::tv-sec time-sec)
                        ('le::tv-usec time-usec))
      (let* ((pointer (cffi:foreign-alloc :char :count 0))
             (ev (le::event-new *event-base* -1 0 (cffi:callback timer-cb) pointer)))
        (save-callbacks pointer :timer-cb callback)
        (le::event-add ev time-c)))))

(defun tcp-async-send (host port data read-cb fail-cb &key ((:socket bev)) (read-timeout 30) (write-timeout 30))
  "Open a TCP connection asynchronously. An event loop must be running for this
   to work."
  (check-event-loop-running)
  (let* ((bev-exists-p bev)
         (bev (if bev
                  bev
                  (le::bufferevent-socket-new *event-base* -1 (cffi:foreign-enum-value 'le::bufferevent-options :+bev-opt-close-on-free+)))))
    (le::bufferevent-setcb bev (cffi:callback tcp-read-cb) (cffi:null-pointer) (cffi:callback tcp-event-cb) *event-base*)
    (le::bufferevent-enable bev (logior le::+ev-read+ le::+ev-write+))
    (save-callbacks bev :read-cb read-cb :fail-cb fail-cb)
    (write-socket-data bev data)
    (set-socket-timeouts bev read-timeout write-timeout)
    ;(make-foreign-type (sockaddr (le::cffi-type le::sockaddr-in) :initial #x0)
    ;                   (('le::sin-family le::+af-inet+)
    ;                    ('le::sin-port (cffi:foreign-funcall "htons" :int port :unsigned-short))
    ;                    ('le::sin-addr (cffi:foreign-funcall "inet_addr" :string "127.0.0.1" :unsigned-long)))
    ;  (le::bufferevent-socket-connect bev
    ;                                  sockaddr
    ;                                  (cffi:foreign-type-size (le::cffi-type le::sockaddr-in))))
    (unless bev-exists-p
      (let ((dns-base (le::evdns-base-new *event-base* 1)))
        (le::bufferevent-socket-connect-hostname bev dns-base le::+af-unspec+ host port)
        (let ((socket (le::bufferevent-getfd bev)))
          (le::evutil-make-socket-nonblocking socket))))))

(defun tcp-async-server (bind-address port read-cb fail-cb)
  "Start a TCP listener on the current event loop."
  (check-event-loop-running)
  (make-foreign-type (sockaddr (le::cffi-type le::sockaddr-in) :initial #x0)
                     (('le::sin-family le::+af-inet+)
                      ('le::sin-port (cffi:foreign-funcall "htons" :int port :unsigned-short))
                      ('le::sin-addr (if bind-address
                                         (cffi:foreign-funcall "inet_addr" :string bind-address :unsigned-long)
                                         (cffi:foreign-funcall "htonl" :unsigned-long 0 :unsigned-long))))
    (let* ((listener (le::evconnlistener-new-bind *event-base*
                                                  (cffi:callback tcp-accept-cb)
                                                  (cffi:null-pointer)
                                                  (logior le::+lev-opt-reuseable+ le::+lev-opt-close-on-free+)
                                                  -1
                                                  sockaddr
                                                  (cffi:foreign-type-size (le::cffi-type le::sockaddr-in)))))
      (when (and (not (cffi:pointerp listener)) (zerop listener))
        (error "Couldn't create listener: ~a~%" listener))
      ;(le::evconnlistener-set-error-cb listener (cffi:callback tcp-accept-err-cb))
      (save-callbacks listener :read-cb read-cb :fail-cb fail-cb)
      (add-event-loop-exit-callback (lambda () (free-listener listener))))))

(defun start-event-loop (start-fn)
  "Simple wrapper function that starts an event loop which runs the given
   callback, most likely to init your server/client."
  (if *event-base*
      (error "Event loop already started. Please wait for it to exit.")
      (let ((*event-base* (le::event-base-new)))
        (timer 0.0 start-fn)
        (le::event-base-dispatch *event-base*)
        (process-event-loop-exit-callbacks)
        (le::event-base-free *event-base*)
        (setf *event-base* nil))))

(defun event-loop-exit ()
  "Exit the event loop if running."
  (if *event-base*
      (le::event-base-loopexit *event-base* (cffi:null-pointer))
      nil))

