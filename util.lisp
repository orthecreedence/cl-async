;;; This file holds some useful helpers for all included systems.

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

(defparameter *ipv4-scanner*
  (cl-ppcre:create-scanner
    "^[0-9]{1,3}(\\.[0-9]{1,3}){3}$"
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

