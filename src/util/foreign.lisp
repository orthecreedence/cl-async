(in-package :cl-async-util)

(defconstant +af-inet+ uv:+af-inet+)
(defconstant +af-inet6+ uv:+af-inet-6+)
(defconstant +af-unspec+ uv:+af-unspec+)
(defconstant +af-unix+ uv:+af-unix+)

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

(defmacro with-foreign-object* ((var type &key (zero t) (initial (when zero #x0))) bindings &body body)
  "Convenience macro, makes creation and initialization of CFFI types easier.
   Emphasis on initialization."
  (let ((type-size (cffi:foreign-type-size (list :struct type))))
    `(cffi:with-foreign-object (,var :unsigned-char ,type-size)
       ,(when initial
          `(cffi:foreign-funcall "memset" :pointer ,var :unsigned-char ,initial :unsigned-char ,(if type-size type-size `(cffi:foreign-type-size '(:struct ,type)))))
       ,@(loop for binding in bindings collect
           `(setf (,(car binding) ,var) ,(cadr binding)))
       ,@body)))

(defun error-str (uv-errno)
  "Given a libuv error number, return the error string."
  ;; a lot of times errors come through that aren't known by libuv, and if we
  ;; ask libuv about it, it aborts (in all its wisdom). these errors seems to
  ;; always be > 0, while the libuv error codes always seem to be < 0.
  (or (ignore-errors (cffi:foreign-enum-keyword 'uv:uv-errno-t uv-errno))
      "(unknown error)"))

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
     (let ((sockaddr (cffi:foreign-alloc '(:struct uv:sockaddr-in6))))
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
  (let* (;(type 'uv:addrinfo)
         (family (uv-a:addrinfo-ai-family addrinfo))
         (err nil))
    (cffi:with-foreign-object (buf :unsigned-char 128)
      (let ((ai-addr (uv-a:addrinfo-ai-addr addrinfo)))
        (if (cffi:null-pointer-p ai-addr)
            (error "the addrinfo->ai_addr object was null (stinks of a memory alignment issue)")
            (cond ((eq family +af-inet+)
                   (let ((sin-addr (cffi:foreign-slot-pointer ai-addr '(:struct uv:sockaddr-in) 'uv:sin-addr)))
                     (uv:uv-inet-ntop family sin-addr buf 128)))
                  ((eq family +af-inet6+)
                   (let ((sin6-addr (cffi:foreign-slot-pointer ai-addr '(:struct uv:sockaddr-in6) 'uv:sin6-addr)))
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

