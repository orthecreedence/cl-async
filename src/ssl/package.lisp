(defpackage :cl-async-ssl
  (:use :cl :cl-async-base :cl-async :cl-async-util)
  (:nicknames :as-ssl)
  (:shadow cl-async-util:exit-event-loop)
  (:export #:+ssl-op-all+
           #:+ssl-op-no-query-mtu+
           #:+ssl-op-cookie-exchange+
           #:+ssl-op-no-ticket+
           #:+ssl-op-cisco-anyconnect+
           #:+ssl-op-no-session-resumption-on-renegotiation+
           #:+ssl-op-no-compression+
           #:+ssl-op-allow-unsafe-legacy-renegotiation+
           #:+ssl-op-single-ecdh-use+
           #:+ssl-op-single-dh-use+
           #:+ssl-op-ephemeral-rsa+
           #:+ssl-op-cipher-server-preference+
           #:+ssl-op-tls-rollback-bug+
           #:+ssl-op-no-sslv2+
           #:+ssl-op-no-sslv3+
           #:+ssl-op-no-tlsv1+
           #:+ssl-op-no-tlsv1-2+
           #:+ssl-op-no-tlsv1-1+

           #:+ssl-verify-none+
           #:+ssl-verify-peer+
           #:+ssl-verify-fail-if-no-peer-cert+
           #:+ssl-verify-client-once+

           #:ssl-socket
           #:tcp-ssl-error
           #:tcp-ssl-connect
           #:tcp-ssl-server)
  (:import-from :cl-async
                #:*output-buffer*
                #:*input-buffer*
                #:event-handler
                #:socket-connected
                #:socket-buffer
                #:socket-buffering-p
                #:socket-direction
                #:check-socket-open
                #:check-event-loop-running
                #:socket-drain-read-buffer
                #:write-to-uvstream
                #:write-socket-data
                #:write-pending-socket-data
                #:init-incoming-socket
                #:stream-append-bytes))
(in-package :cl-async-ssl)

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

  (cffi:use-foreign-library libeay32))
