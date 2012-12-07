;;; This houses all common funcitonality/utilities for cl-async used throughout
;;; the various extending packages. This package is meant to be used internally
;;; only.

(defpackage :cl-async-util
  (:use :cl)
  (:export #:+af-inet+
           #:+af-inet6+
           #:+af-unspec+
           #:+af-unix+

           #:+sockaddr-size+
           #:+sockaddr6-size+
           #:+addrinfo-size+
           #:+timeval-size+
           #:+bev-opt-close-on-free+

           #:*event-base*
           #:*event-base-id*
           #:*fn-registry*
           #:*data-registry*
           #:*event-loop-end-functions*
           #:*dns-base*
           #:*dns-ref-count*

           #:*buffer-size*
           #:*socket-buffer-c*
           #:*socket-buffer-lisp*

           #:*incoming-connection-count*
           #:*outgoing-connection-count*
           #:*incoming-http-count*
           #:*outgoing-http-count*

           #:*catch-application-errors*

           #:*signal-handlers*
           #:*default-event-handler*
           #:catch-app-errors

           #:make-foreign-type

           #:make-pointer-eql-able
           #:create-data-pointer
           #:save-callbacks
           #:get-callbacks
           #:clear-callbacks
           #:attach-data-to-pointer
           #:deref-data-from-pointer
           #:clear-pointer-data
           #:free-pointer-data

           #:split-usec-time

           #:append-array

           #:add-event-loop-exit-callback
           #:process-event-loop-exit-callbacks
           #:check-event-loop-running

           #:*ipv4-scanner*
           #:*ipv6-scanner*

           #:ipv4-address-p
           #:ipv6-address-p
           #:ip-address-p
           #:ip-str-to-sockaddr
           #:with-ip-to-sockaddr
           #:win32-switch
           #:*addrinfo*
           #:addrinfo-ai-addr))
(in-package :cl-async-util)

(defconstant +af-inet+ le:+af-inet+)
(defconstant +af-inet6+ le:+af-inet-6+)
(defconstant +af-unspec+ le:+af-unspec+)
(defconstant +af-unix+ le:+af-unix+)

;; define some cached values to save CFFI calls. believe it or not, this does
;; make a performance difference
(defconstant +sockaddr-size+ (cffi:foreign-type-size (le::cffi-type le::sockaddr-in)))
(defconstant +sockaddr6-size+ (cffi:foreign-type-size (le::cffi-type le::sockaddr-in-6)))
(defconstant +addrinfo-size+ (cffi:foreign-type-size (le::cffi-type le::addrinfo)))
(defconstant +timeval-size+ (cffi:foreign-type-size (le::cffi-type le::timeval)))
(defconstant +bev-opt-close-on-free+ (cffi:foreign-enum-value 'le:bufferevent-options :+bev-opt-close-on-free+))

(defvar *event-base* nil
  "THE event base (libevent) used to process all async operations.")
(defvar *event-base-id* 0
  "The numeric identifier assigned to each new event base.")
(defvar *fn-registry* nil
  "Function registry, allows the CFFI callbacks to run anonymous functions.")
(defvar *data-registry* nil
  "Data registry, gives CFFI callbacks access to anonymous data.")
(defvar *event-loop-end-functions* nil
  "Functions to call when the event loop closes")

(defvar *dns-base* nil
  "Holds the evdns-base object used for DNS lookups. One per event loop should
   suffice.")

(defvar *dns-ref-count* 0
  "Counts how many open DNS queries there are, and allows freeing the DNS base
   once there are no more references.")

(defparameter *buffer-size* 16384
  "The amount of data we'll pull from the evbuffers when doing reading/writing.")
(defvar *socket-buffer-c* nil
  "A pointer to the buffer in C land that reads from sockets.")
(defvar *socket-buffer-lisp* nil
  "An array in lisp land that holds data copied from a socket.")

(defvar *incoming-connection-count* 0
  "Number of incoming TCP connections.")
(defvar *outgoing-connection-count* 0
  "Number of outgoing TCP connections.")
(defvar *incoming-http-count* 0
  "Number of incoming HTTP connections.")
(defvar *outgoing-http-count* 0
  "Number of outgoing HTTP connections.")

(defvar *catch-application-errors* nil
  "When t, permits cl-async to catch uncaught conditions in your application and
   pass them to the event-cb callback given. If no event-cb is given for the
   operation that triggered the condition, use *default-event-handler* as the
   event-cb.")

(defvar *signal-handlers* nil
  "Holds all the currently bound signal handlers, which can be used to unbind
   them all in one swift stroke.")

(defvar *default-event-handler*
  (lambda (err)
    ;; throw the error so we can wrap it in a handler-case
    (handler-case (error err)
      ;; got a connection error, throw it (must do this explicitely since
      ;; connection-error extends connection-info)
      (connection-error () (error err))

      ;; this is just info, let it slide
      (connection-info () nil)
      
      ;; this an actual error. throw it back to toplevel
      (t () (error err))))
  "If an event-cb is not specified, this will be used as the event-cb IF
   *catch-application-errors* is set to t.")

(defmacro catch-app-errors (event-cb &body body)
  "Wraps catching of application errors into a simple handler-case (if wanted),
   otherwise just runs the body with no error/event handling."
  (let ((evcb (gensym)))
    `(if *catch-application-errors*
         (let ((,evcb (if (functionp ,event-cb)
                          ,event-cb
                          *default-event-handler*)))
           (handler-case
             (progn ,@body)
             (t (err) (funcall ,evcb err))))
         (progn ,@body))))
     
(defmacro make-foreign-type ((var type &key initial type-size) bindings &body body)
  "Convenience macro, makes creation and initialization of CFFI types easier.
   Emphasis on initialization."
  `(cffi:with-foreign-object (,var ,type)
     ,(when initial
        `(cffi:foreign-funcall "memset" :pointer ,var :unsigned-char ,initial :unsigned-char ,(if type-size type-size `(cffi:foreign-type-size ,type))))
     ,@(loop for binding in bindings collect
         `(setf (cffi:foreign-slot-value ,var ,type ,(car binding)) ,(cadr binding)))
     ,@body))

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
  (unless *fn-registry*
    (setf *fn-registry* (make-hash-table :test #'eql)))
  (let ((callbacks (if (listp callbacks)
                       callbacks
                       (list callbacks))))
    (setf (gethash (make-pointer-eql-able pointer) *fn-registry*) callbacks)))

(defun get-callbacks (pointer)
  "Get all callbacks for the given pointer."
  (when *fn-registry*
    (gethash (make-pointer-eql-able pointer) *fn-registry*)))

(defun clear-callbacks (pointer)
  "Clear out all callbacks for the given pointer."
  (when *fn-registry*
    (remhash (make-pointer-eql-able pointer) *fn-registry*)))

(defun attach-data-to-pointer (pointer data)
  "Attach a lisp object to a foreign pointer."
  (unless *data-registry*
    (setf *data-registry* (make-hash-table :test #'eql)))
  (setf (gethash (make-pointer-eql-able pointer) *data-registry*) data))

(defun deref-data-from-pointer (pointer)
  "Grab data attached to a CFFI pointer."
  (when (and pointer *data-registry*)
    (gethash (make-pointer-eql-able pointer) *data-registry*)))

(defun clear-pointer-data (pointer)
  "Clear the data attached to a CFFI pointer."
  (when (and pointer *data-registry*)
    (remhash (make-pointer-eql-able pointer) *data-registry*)))

(defun free-pointer-data (pointer &key preserve-pointer)
  "Clears out all data attached to a foreign pointer, and frees the pointer
   (unless :preserve-pointer is t)."
  (when pointer
    (unwind-protect
      (progn
        (clear-callbacks pointer)
        (clear-pointer-data pointer))
      (unless preserve-pointer
        (when (cffi:pointerp pointer)
          (cffi:foreign-free pointer))))))

(defun split-usec-time (time-s)
  "Given a second value, ie 3.67, return the number of seconds as the first
   value and the number of usecs for the second value."
  (if (numberp time-s)
      (multiple-value-bind (time-sec time-frac) (floor time-s)
        (values time-sec (floor (* 1000000 time-frac))))
      nil))

(defun append-array (arr1 arr2)
  "Create an array, made up of arr1 followed by arr2."
  (let ((arr1-length (length arr1))
        (arr2-length (length arr2)))
    (let ((arr (make-array (+ arr1-length arr2-length)
                           :element-type (array-element-type arr1))))
      (replace arr arr1 :start1 0)
      (replace arr arr2 :start1 arr1-length)
      arr)))
      
(defun add-event-loop-exit-callback (fn)
  "Add a function to be run when the event loop exits."
  (push fn *event-loop-end-functions*))

(defun process-event-loop-exit-callbacks ()
  "run and clear out all event loop exit functions."
  (dolist (fn *event-loop-end-functions*)
    (funcall fn))
  (setf *event-loop-end-functions* nil))

(defun check-event-loop-running ()
  (unless *event-base*
    (error "Event loop not running. Start with function start-event-loop.")))

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
     (let ((sockaddr (cffi:foreign-alloc (le::cffi-type le::sockaddr-in))))
       ;; fill it full of holes.
       (cffi:foreign-funcall "memset" :pointer sockaddr :unsigned-char 0 :unsigned-char +sockaddr-size+)
       (setf (le-a:sockaddr-in-sin-family sockaddr) +af-inet+
             (le-a:sockaddr-in-sin-port sockaddr) (cffi:foreign-funcall "htons" :int port :unsigned-short)
             (le-a:sockaddr-in-sin-addr sockaddr) (if address
                                                      (cffi:foreign-funcall "inet_addr" :string address :unsigned-long)
                                                      (cffi:foreign-funcall "htonl" :unsigned-long 0 :unsigned-long)))
       (values sockaddr +sockaddr-size+)))
    ((ipv6-address-p address)
     (let ((sockaddr6 (cffi:foreign-alloc (le::cffi-type le::sockaddr-in-6))))
       (cffi:foreign-funcall "memset" :pointer sockaddr6 :unsigned-char 0 :unsigned-char +sockaddr6-size+)
       (setf (le-a:sockaddr-in-6-sin-6-family sockaddr6) +af-inet6+
             (le-a:sockaddr-in-6-sin-6-port sockaddr6) (cffi:foreign-funcall "htons" :int port :unsigned-short))
       (cffi:foreign-funcall "inet_pton"
                             :short +af-inet6+
                             :string address
                             :pointer (cffi:foreign-slot-pointer sockaddr6 (le::cffi-type le::sockaddr-in-6) 'le::sin-6-addr-0))
       (values sockaddr6 +sockaddr6-size+)))
    (t
     (error (format nil "Invalid address passed (not IPv4 or IPV6): ~s~%" address)))))

(defmacro with-ip-to-sockaddr (((bind bind-size) address port) &body body)
  "Wraps around ipv4-str-to-sockaddr. Converts a string address and port and
   creates a sockaddr-in object, runs the body with it bound, and frees it."
  `(multiple-value-bind (,bind ,bind-size) (ip-str-to-sockaddr ,address ,port)
     (unwind-protect
       (progn ,@body)
       (cffi:foreign-free ,bind))))


;; fix some broken shit with addrinfo on windows vs linux...
(defmacro win32-switch (form-if-win form-if-not-win)
  "Given two forms, returns the first if we're running on windows, and the
   second form otherwise."
  #+(or windows win32)
    form-if-win
  #-(or windows win32)
    form-if-not-win)

(defparameter *addrinfo*
  (win32-switch
    (le::cffi-type le::evutil-addrinfo)
    (le::cffi-type le::addrinfo))
  "Determines the correct type of addrinfo for the current platform.")

(defmacro addrinfo-ai-addr (pt)
  "A wrapper around addrinfo's ai-addr accessor (there is one for windows that
   uses evutil_addrinfo, and one for linux that uses addrinfo)."
  (win32-switch
    `(le-a:evutil-addrinfo-ai-addr ,pt)
    `(le-a:addrinfo-ai-addr ,pt)))

