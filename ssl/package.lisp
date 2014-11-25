(defpackage :cl-async-ssl
  (:use :cl :cl-async-base :cl-async :cl-async-util)
  (:nicknames :as-ssl)
  (:export #:ssl-socket
           #:tcp-ssl-server
           #:socket-underlying
           #:tcp-ssl-error
           ;#:wrap-in-ssl
           #:init-tcp-ssl-socket
           #:tcp-ssl-connect
           #:tcp-ssl-server)
  (:import-from :cl-async
                #:*output-buffer*
                #:*input-buffer*
                #:socket-connected
                #:socket-buffer
                #:socket-buffering-p
                #:check-socket-open
                #:check-event-loop-running
                #:socket-drain-read-buffer
                #:write-to-uvstream
                #:write-socket-data
                #:write-pending-socket-data
                #:init-incoming-socket
                #:tcp-server-c
                #:stream-append-bytes))

;; NOTE: the loading code is verbatim from cl+ssl

(eval-when (:compile-toplevel :load-toplevel)
  ;; OpenBSD needs to load libcrypto before libssl
  #+openbsd
  (progn
    (cffi:define-foreign-library libcrypto
      (:openbsd (:or "libcrypto.so.20.1"
                     "libcrypto.so.19.0"
                     "libcrypto.so.18.0")))
    (cffi:use-foreign-library libcrypto))
  
  (cffi:define-foreign-library libssl
    (:windows "libssl32.dll")
    (:darwin "libssl.dylib")
    (:openbsd (:or "libssl.so.18.0" "libssl.so.17.1"
                   "libssl.so.16.0" "libssl.so.15.1"))
    (:solaris (:or "/lib/64/libssl.so"
                   "libssl.so.0.9.8" "libssl.so" "libssl.so.4"))
    (:unix (:or "libssl.so.1.0.0" "libssl.so.0.9.8" "libssl.so" "libssl.so.4"))
    (:cygwin "cygssl-1.0.0.dll")
    (t (:default "libssl3")))
  
  (cffi:use-foreign-library libssl)
  
  (cffi:define-foreign-library libeay32
    (:windows "libeay32.dll"))
  
  (cffi:use-foreign-library libeay32)

  (cffi:foreign-funcall "SSL_library_init" :void)
  (cffi:foreign-funcall "SSL_load_error_strings" :void)
  (cffi:foreign-funcall "ERR_load_BIO_strings" :void))

