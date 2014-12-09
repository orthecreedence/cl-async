;;; This houses all common funcitonality/utilities for cl-async used throughout
;;; the various extending packages. This package is meant to be used internally
;;; only.

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

           #:call-with-callback-restarts
           #:catch-app-errors
           #:run-event-cb

           #:define-c-callback

           #:make-foreign-type

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
           #:fd-connected-p))
(in-package :cl-async-util)

(deftype octet () '(unsigned-byte 8))
(deftype octet-vector () '(simple-array octet (*)))
(deftype bytes-or-string () '(or octet-vector string))
(deftype callback () '(or null function symbol))

(defun bytes (vector)
  "Convert any vector/string into a byte array. Useful for sending direct byte
   data into write-socket-data."
  (coerce vector '(vector octet)))

(defun make-buffer (&optional data)
  "Create an octet buffer, optoinally filled with the given data."
  (declare (type (or null octet-vector) data))
  (let ((buffer (fast-io:make-output-buffer)))
    (when data
      (write-to-buffer data buffer))
    buffer))

(declaim (inline buffer-output))
(defun buffer-output (buffer)
  "Grab the output from a buffer created with (make-buffer)."
  (declare (type fast-io::output-buffer buffer))
  (fast-io:finish-output-buffer buffer))

(declaim (inline write-to-buffer))
(defun write-to-buffer (seq buffer &optional start end)
  "Write data to a buffer created with (make-buffer)."
  (declare (type octet-vector seq)
           (type fast-io::output-buffer buffer))
  (fast-io:fast-write-sequence seq buffer (or start 0) end))

(defun do-chunk-data (data buffer write-cb &key start end)
  "Util function that splits data into the (length buffer) chunks and calls
   write-cb for each chunk."
  (let* ((len (length data))
         (start (or start 0))
         (end (min (or end len) len))
         (data-length (- end start))
         (data-index start)
         (buffer-length (length buffer)))
    (loop while (< 0 data-length) do
      (let ((bufsize (min data-length buffer-length)))
        (replace buffer data :start2 data-index :end2 end)
        (funcall write-cb buffer bufsize)
        (decf data-length bufsize)
        (incf data-index bufsize)))))

(defconstant +af-inet+ uv:+af-inet+)
(defconstant +af-inet6+ uv:+af-inet-6+)
(defconstant +af-unspec+ uv:+af-unspec+)
(defconstant +af-unix+ uv:+af-unix+)

;; define some cached values to save CFFI calls. believe it or not, this does
;; make a performance difference
(defconstant +sockaddr-size+ (cffi:foreign-type-size '(:struct uv:sockaddr-in)))
(defconstant +sockaddr6-size+ (cffi:foreign-type-size '(:struct uv:sockaddr-in-6)))
(defconstant +addrinfo-size+ (cffi:foreign-type-size '(:struct uv:addrinfo)))

(defun call-with-callback-restarts (thunk &optional (abort-restart-description "Abort cl-async callback."))
  "Call thunk with restarts that make it possible to ignore the callback
  in case of an error or safely terminate the event loop.

  If SWANK is active, set SLDB's quit restart to ABORT-CALLBACK restart
  if *safe-sldb-quit-restart* is true or EXIT-EVENT-LOOP otherwise."
  (restart-case
      ;; have to use the ugly symbol-value hack instead of #+swank / #-swank
      ;; in order to avoid compiling in SWANK (in)dependency
      (let* ((swank-package (find-package :swank))
             (quit-restart-sym (when swank-package
                                 (find-symbol (symbol-name '#:*sldb-quit-restart*)
                                              swank-package))))
        (if quit-restart-sym
            (let ((old-quit-restart (symbol-value quit-restart-sym)))
              (setf (symbol-value quit-restart-sym)
                    (if *safe-sldb-quit-restart*
                        'abort-callback
                        'exit-event-loop))
              (unwind-protect
                   (funcall thunk)
                (setf (symbol-value quit-restart-sym) old-quit-restart)))
            (funcall thunk)))
    (abort-callback ()
      :report (lambda (stream) (write-string abort-restart-description stream))
      (format *debug-io* "~&;; callback aborted~%")
      (values))
    (exit-event-loop ()
      :report "Exit the current event loop"
      (format *debug-io* "~&;; exiting the event loop.~%")
      (uv:uv-stop (event-base-c *event-base*)))))

(defmacro catch-app-errors (event-cb &body body)
  "Wraps catching of application errors into a simple handler-case (if wanted),
   otherwise just runs the body with no error/event handling.

   If event-cbs are called via run-event-cb, makes sure the event-cb is NOT
   double-called with the same condition twice."
  (let ((evcb (gensym "evcb")))
    ;; define a binding for tracking already-fired errors. run-event-cb will
    ;; use this binding
    `(let ((_evcb-err nil))
       (if (event-base-catch-app-errors *event-base*)
           (let* ((,evcb (if (symbolp ,event-cb)
                             (handler-case (symbol-function ,event-cb)
                               (undefined-function () nil))
                             ,event-cb))
                  (,evcb (if (functionp ,evcb)
                             ,evcb
                             (event-base-default-event-handler *event-base*))))
             (handler-case
               (progn ,@body)
               (t (err)
                 (if (equal err _evcb-err)
                     ;; error was already sent to eventcb, retrigger
                     (error err)
                     ;; what do you know, a new error. send to event-cb =]
                     (funcall ,evcb err)))))
           (call-with-callback-restarts #'(lambda () ,@body))))))

(defmacro run-event-cb (event-cb &rest args)
  "Used inside of catch-app-errors, wraps the calling of an event-cb such that
   errors are caught and saved, making it so an event-cb isn't called twice with
   the same condition."
  `(handler-case
     ;; run the event handler
     (funcall ,event-cb ,@args)
     ;; catch any errors and track them
     (t (e)
       ;; track the error so we don't re-fire (_evcb-err is defined in
       ;; catch-app-errors)
       (setf _evcb-err e)
       (error e))))

(defmacro define-c-callback (name return-val (&rest args) &body body)
  "Define a top-level function with the given and also define a C callback that
   calls the function directly. The idea is that CFFI callbacks aren't directly
   callable/debuggable, but it's obnoxious to have to define and callback *and*
   a function right next to each other."
  (let ((arg-names (loop for x in args collect (car x))))
    `(progn
       (defun ,name ,arg-names
         ,@body)
       (cffi:defcallback ,name ,return-val ,args
         (,name ,@arg-names)))))

(defmacro make-foreign-type ((var type &key initial) bindings &body body)
  "Convenience macro, makes creation and initialization of CFFI types easier.
   Emphasis on initialization."
  (let ((type (if (eq type 'uv:addrinfo)
                  (progn
                    #+windows 'uv:addrinfo-w
                    #-windows 'uv:addrinfo)
                  type))
        (type-size (cffi:foreign-type-size (list :struct type))))
    `(cffi:with-foreign-object (,var :unsigned-char ,type-size)
       ,(when initial
          `(cffi:foreign-funcall "memset" :pointer ,var :unsigned-char ,initial :unsigned-char ,(if type-size type-size `(cffi:foreign-type-size '(:struct ,type)))))
       ,@(loop for binding in bindings collect
           `(setf (cffi:foreign-slot-value ,var '(:struct ,type) ,(car binding)) ,(cadr binding)))
       ,@body)))

(defmacro with-lock (&body body)
  "If threading is enabled, locks the current event loop before processing body
   and releases the lock after body is finished."
  `(if *enable-threading*
       (bt:with-lock-held ((event-base-lock *event-base*))
         ,@body)
       (progn ,@body)))

(defun make-pointer-eql-able (pointer)
  "Abstraction to make a CFFI pointer #'eql to itself. Does its best to be the
   most performant for the current implementation."
  (when pointer
    #+(or ccl)
      pointer
    #-(or ccl)
      (if (cffi:pointerp pointer)
          (cffi:pointer-address pointer)
          pointer)))

(defun create-data-pointer ()
  "Creates a pointer in C land that can be used to attach data/callbacks to.
   Note that this must be freed via clear-pointer-data."
  (cffi:foreign-alloc :char :count 1))

(defun save-callbacks (pointer callbacks)
  "Save a set of callbacks, keyed by the given pointer."
  (with-lock
    (let ((callbacks (if (listp callbacks)
                         callbacks
                         (list callbacks))))
      (setf (gethash (make-pointer-eql-able pointer) *function-registry*) callbacks))))

(defun get-callbacks (pointer)
  "Get all callbacks for the given pointer."
  (with-lock
    (when *function-registry*
      (gethash (make-pointer-eql-able pointer) *function-registry*))))

(defun clear-callbacks (pointer)
  "Clear out all callbacks for the given pointer."
  (with-lock
    (when *function-registry*
      (remhash (make-pointer-eql-able pointer) *function-registry*))))

(defun attach-data-to-pointer (pointer data)
  "Attach a lisp object to a foreign pointer."
  (with-lock
    (setf (gethash (make-pointer-eql-able pointer) *data-registry*) data)))

(defun deref-data-from-pointer (pointer)
  "Grab data attached to a CFFI pointer."
  (with-lock
    (when (and pointer *data-registry*)
      (gethash (make-pointer-eql-able pointer) *data-registry*))))

(defun clear-pointer-data (pointer)
  "Clear the data attached to a CFFI pointer."
  (with-lock
    (when (and pointer *data-registry*)
      (remhash (make-pointer-eql-able pointer) *data-registry*))))

(defun free-pointer-data (pointer &key preserve-pointer)
  "Clears out all data attached to a foreign pointer, and frees the pointer
   (unless :preserve-pointer is t)."
  (when pointer
    (unwind-protect
      (progn
        (clear-callbacks pointer)
        (clear-pointer-data pointer))
      (unless preserve-pointer
        (with-lock
          (when (cffi:pointerp pointer)
            (cffi:foreign-free pointer)))))))

(defun append-array (arr1 arr2)
  "Create an array, made up of arr1 followed by arr2."
  (let ((arr1-length (length arr1))
        (arr2-length (length arr2)))
    (let ((arr (make-array (+ arr1-length arr2-length)
                           :element-type (array-element-type arr1))))
      (replace arr arr1 :start1 0)
      (replace arr arr2 :start1 arr1-length)
      arr)))

(defun error-str (uv-errno)
  "Given a libuv error number, return the error string."
  (uv:uv-err-name uv-errno))

(defparameter *ipv4-scanner*
  (cl-ppcre:create-scanner
    "^((25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[0-9]{2}|[0-9])\\.){3}(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[0-9]{2}|[0-9])$"
    :case-insensitive-mode t)
  "Scanner that detects if a string is an IPV4 address.")

(defparameter *ipv6-scanner*
  (cl-ppcre:create-scanner
    "^\s*((([0-9A-Fa-f]{1,4}:){7}([0-9A-Fa-f]{1,4}|:))|(([0-9A-Fa-f]{1,4}:){6}(:[0-9A-Fa-f]{1,4}|((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9A-Fa-f]{1,4}:){5}(((:[0-9A-Fa-f]{1,4}){1,2})|:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9A-Fa-f]{1,4}:){4}(((:[0-9A-Fa-f]{1,4}){1,3})|((:[0-9A-Fa-f]{1,4})?:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){3}(((:[0-9A-Fa-f]{1,4}){1,4})|((:[0-9A-Fa-f]{1,4}){0,2}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){2}(((:[0-9A-Fa-f]{1,4}){1,5})|((:[0-9A-Fa-f]{1,4}){0,3}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){1}(((:[0-9A-Fa-f]{1,4}){1,6})|((:[0-9A-Fa-f]{1,4}){0,4}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(:(((:[0-9A-Fa-f]{1,4}){1,7})|((:[0-9A-Fa-f]{1,4}){0,5}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:)))(%.+)?\s*$"
    :case-insensitive-mode t)
  "Scanner that detects if a string is an IPV6 address.")

(defun ipv4-address-p (addr)
  "Determine if the given host is an IPV4 addr or a hostname."
  (cl-ppcre:scan *ipv4-scanner* addr))

(defun ipv6-address-p (addr)
  "Determine if the given host is an IPV6 addr or a hostname."
  (cl-ppcre:scan *ipv6-scanner* addr))

(defun ip-address-p (addr)
  "Determine if the given host is an IP or a hostname."
  (or (ipv4-address-p addr)
      (ipv6-address-p addr)))

(defun ip-str-to-sockaddr (address port)
  "Convert a string IP address and port into a sockaddr-in struct. Must be freed
   by the app!"
  (cond
    ((or (null address)
         (ipv4-address-p address))
     (let ((sockaddr (cffi:foreign-alloc '(:struct uv:sockaddr-in))))
       (uv:uv-ip-4-addr (or address "0.0.0.0") port sockaddr)
       sockaddr))
    ((ipv6-address-p address)
     (let ((sockaddr (cffi:foreign-alloc '(:struct uv:sockaddr-in-6))))
       (uv:uv-ip-6-addr address port sockaddr)
       sockaddr))
    (t
     (error (format nil "Invalid address passed (not IPv4 or IPV6): ~s~%" address)))))

(defmacro with-ip-to-sockaddr (((bind) address port) &body body)
  "Wraps around ipv4-str-to-sockaddr. Converts a string address and port and
   creates a sockaddr-in object, runs the body with it bound, and frees it."
  `(let ((,bind (ip-str-to-sockaddr ,address ,port)))
     (unwind-protect
       (progn ,@body)
       (cffi:foreign-free ,bind))))

(defun addrinfo-to-string (addrinfo)
  "Given a (horrible) addrinfo C object pointer, grab either an IP4 or IP6
   address and return is as a string."
  (let* ((type (progn #+windows 'uv:addrinfo-w #-windows 'uv:addrinfo))
         (family (cffi:foreign-slot-value addrinfo (list :struct type) 'uv::ai-family))
         (err nil))
    (cffi:with-foreign-object (buf :unsigned-char 128)
      ;; note here, we use the OS-dependent addrinfo-ai-addr macro
      ;; defined in util.lisp
      (let ((ai-addr (cffi:foreign-slot-value addrinfo (list :struct type) 'uv::ai-addr)))
        (if (cffi:null-pointer-p ai-addr)
            (setf err "the addrinfo->ai_addr object was null (stinks of a memory alignment issue)")
            (cond ((eq family +af-inet+)
                   (let ((sin-addr (cffi:foreign-slot-pointer ai-addr '(:struct uv:sockaddr-in) 'uv::sin-addr)))
                     (uv:uv-inet-ntop family sin-addr buf 128)))
                  ((eq family +af-inet6+)
                   (let ((sin6-addr (cffi:foreign-slot-pointer ai-addr '(:struct uv:sockaddr-in-6) 'uv::sin-6-addr-0)))
                     (uv:uv-inet-ntop family sin6-addr buf 128)))
                  (t
                   (setf err (format nil "unsupported DNS family: ~a" family))))))
      (values (cffi:foreign-string-to-lisp buf) family err))))

(defun set-socket-nonblocking (fd)
  "Sets an FD into non-blocking mode."
  (let ((FIONBIO -2147195266)
        (F_GETFL 3)
        (F_SETFL 4)
        (O_NONBLOCK 2048))
    (cond ((cffi:foreign-symbol-pointer "ioctlsocket")
           (cffi:with-foreign-object (nonblocking :unsigned-long)
             (setf (cffi:mem-aref nonblocking :unsigned-long) 1)
             (cffi:foreign-funcall "ioctlsocket"
                                   :int fd
                                   :long FIONBIO
                                   :pointer nonblocking
                                   :int)))
          ((cffi:foreign-symbol-pointer "fcntl")
           (let ((flags (cffi:foreign-funcall "fcntl"
                                              :int fd
                                              :int F_GETFL
                                              :pointer (cffi:null-pointer)
                                              :int)))
             (cffi:foreign-funcall "fcntl"
                                   :int fd
                                   :int F_SETFL
                                   :int (logior flags O_NONBLOCK)
                                   :int))))))

(defun fd-connected-p (fd)
  "Check if an FD is connected."
  (cffi:with-foreign-objects ((error :int)
                              (len :int))
    (setf (cffi:mem-aref len :int) (cffi:foreign-type-size :int))
    (let* ((SOL_SOCKET #+windows 65535 #-windows 1)
           (SO_ERROR #+windows 4103 #-windows 4)
           (res (cffi:foreign-funcall "getsockopt"
                                      :int fd
                                      :int SOL_SOCKET
                                      :int SO_ERROR
                                      :pointer error
                                      :pointer len
                                      :int)))
      (zerop res))))
