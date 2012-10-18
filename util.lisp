;;; This file holds some useful helpers for all included systems.

(in-package :cl-async)

;; define some cached values to save CFFI calls. believe it or not, this does
;; make a performance difference
(defconstant +sockaddr-size+ (cffi:foreign-type-size (le::cffi-type le::sockaddr-in)))
(defconstant +sockaddr6-size+ (cffi:foreign-type-size (le::cffi-type le::sockaddr-in-6)))
(defconstant +evutil-addrinfo-size+ (cffi:foreign-type-size (le::cffi-type le::evutil-addrinfo)))
(defconstant +timeval-size+ (cffi:foreign-type-size (le::cffi-type le::timeval)))
(defconstant +bev-opt-close-on-free+ (cffi:foreign-enum-value 'le:bufferevent-options :+bev-opt-close-on-free+))

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
       (setf (le-a:sockaddr-in-sin-family sockaddr) le:+af-inet+
             (le-a:sockaddr-in-sin-port sockaddr) (cffi:foreign-funcall "htons" :int port :unsigned-short)
             (le-a:sockaddr-in-sin-addr sockaddr) (if address
                                                      (cffi:foreign-funcall "inet_addr" :string address :unsigned-long)
                                                      (cffi:foreign-funcall "htonl" :unsigned-long 0 :unsigned-long)))
       (values sockaddr +sockaddr-size+)))
    ((ipv6-address-p address)
     (let ((sockaddr6 (cffi:foreign-alloc (le::cffi-type le::sockaddr-in-6))))
       (cffi:foreign-funcall "memset" :pointer sockaddr6 :unsigned-char 0 :unsigned-char +sockaddr6-size+)
       (setf (le-a:sockaddr-in-6-sin-6-family sockaddr6) le:+af-inet-6+
             (le-a:sockaddr-in-6-sin-6-port sockaddr6) (cffi:foreign-funcall "htons" :int port :unsigned-short))
       (cffi:foreign-funcall "inet_pton"
                             :short le:+af-inet-6+
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


