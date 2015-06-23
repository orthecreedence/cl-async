;;; this file is sort of our makeshift openssl bindings. i'd generate them as i
;;; usually do via swig, but kind of don't care to because we're only going to
;;; use a fraction of the exported functionality.

(in-package :cl-async-ssl)

(defun zero-buffer (buff)
  "Zero out a buffer. Not 100% perfect because of the GC, but better than
   letting stale plaintext data sit around."
  (dotimes (i (length buff))
    (setf (aref buff i) 0)))

(defconstant +ssl-error-none+ 0)
(defconstant +ssl-error-ssl+ 1)
(defconstant +ssl-error-want-read+ 2)
(defconstant +ssl-error-want-write+ 3)
(defconstant +ssl-error-want-x509-lookup+ 4)
(defconstant +ssl-error-syscall+ 5)
(defconstant +ssl-error-zero-return+ 6)
(defconstant +ssl-error-want-connect+ 7)
(defconstant +ssl-error-want-accept+ 8)

(defconstant +ssl-st-connect+ #x1000)
(defconstant +ssl-st-accept+ #x2000)
(defconstant +ssl-st-mask+ #x0FFF)
(defconstant +ssl-st-init+ (logior +ssl-st-connect+ +ssl-st-accept+))
(defconstant +ssl-st-before+ #x4000)
(defconstant +ssl-st-ok+ #x03)
(defconstant +ssl-st-renegotiate+ (logior #x04 +ssl-st-init+))

(defconstant +ssl-cb-loop+ #x01)
(defconstant +ssl-cb-exit+ #x02)
(defconstant +ssl-cb-read+ #x04)
(defconstant +ssl-cb-write+ #x08)
(defconstant +ssl-cb-alert+ #x4000)
(defconstant +ssl-cb-read-alert+ (logior +ssl-cb-alert+ +ssl-cb-read+))
(defconstant +ssl-cb-write-alert+ (logior +ssl-cb-alert+ +ssl-cb-write+))
(defconstant +ssl-cb-accept-loop+ (logior +ssl-st-accept+ +ssl-cb-loop+))
(defconstant +ssl-cb-accept-exit+ (logior +ssl-st-accept+ +ssl-cb-exit+))
(defconstant +ssl-cb-connect-loop+ (logior +ssl-st-connect+ +ssl-cb-loop+))
(defconstant +ssl-cb-connect-exit+ (logior +ssl-st-connect+ +ssl-cb-exit+))
(defconstant +ssl-cb-handshake-start+ #x10)
(defconstant +ssl-cb-handshake-done+ #x20)
(defconstant +ssl-received-shutdown+ 2)

(defconstant +ssl-verify-none+ #x00)
(defconstant +ssl-verify-peer+ #x01)
(defconstant +ssl-verify-fail-if-no-peer-cert+ #x02)
(defconstant +ssl-verify-client-once+ #x04)

(defconstant +ssl-filetype-asn1+ 2)
(defconstant +ssl-filetype-pem+ 1)
(defconstant +ssl-x509-filetype-default+ 3)

(defconstant +ssl-op-all+ #x80000BFF)
;; DTLS options
(defconstant +ssl-op-no-query-mtu+ #x00001000)
;; Turn on Cookie Exchange (on relevant for servers)
(defconstant +ssl-op-cookie-exchange+ #x00002000)
;; Don't use RFC4507 ticket extension
(defconstant +ssl-op-no-ticket+ #x00004000)
;; Use Cisco's "speshul" version of DTLS_BAD_VER (as client)
(defconstant +ssl-op-cisco-anyconnect+ #x00008000)
;; As server, disallow session resumption on renegotiation
(defconstant +ssl-op-no-session-resumption-on-renegotiation+ #x00010000)
;; Don't use compression even if supported
(defconstant +ssl-op-no-compression+ #x00020000)
;; Permit unsafe legacy renegotiation
(defconstant +ssl-op-allow-unsafe-legacy-renegotiation+ #x00040000)
;; If set, always create a new key when using tmp_ecdh parameters
(defconstant +ssl-op-single-ecdh-use+ #x00080000)
;; If set, always create a new key when using tmp_dh parameters
(defconstant +ssl-op-single-dh-use+ #x00100000)
;; Set to always use the tmp_rsa key when doing RSA operations,
;; even when this violates protocol specs
(defconstant +ssl-op-ephemeral-rsa+ #x00200000)
;; Set on servers to choose the cipher according to the server's
;; preferences
(defconstant +ssl-op-cipher-server-preference+ #x00400000)
;; If set, a server will allow a client to issue a SSLv3.0 version number
;; as latest version supported in the premaster secret, even when TLSv1.0
;; (version 3.1) was announced in the client hello. Normally this is
;; forbidden to prevent version rollback attacks.
(defconstant +ssl-op-tls-rollback-bug+ #x00800000)

(defconstant +ssl-op-no-sslv2+ #x01000000)
(defconstant +ssl-op-no-sslv3+ #x02000000)
(defconstant +ssl-op-no-tlsv1+ #x04000000)
(defconstant +ssl-op-no-tlsv1-2+ #x08000000)
(defconstant +ssl-op-no-tlsv1-1+ #x10000000)

(defconstant +ssl-ctrl-options+ 32)

(defconstant +bio-ctrl-reset+ 1)
(defconstant +bio-ctrl-eof+ 2)
(defconstant +bio-ctrl-info+ 3)
(defconstant +bio-ctrl-set+ 4)
(defconstant +bio-ctrl-get+ 5)
(defconstant +bio-ctrl-push+ 6)
(defconstant +bio-ctrl-pop+ 7)
(defconstant +bio-ctrl-get-close+ 8)
(defconstant +bio-ctrl-set-close+ 9)
(defconstant +bio-ctrl-pending+ 10)
(defconstant +bio-ctrl-flush+ 11)
(defconstant +bio-ctrl-dup+ 12)
(defconstant +bio-ctrl-wpending+ 13)
(defconstant +bio-ctrl-set-callback+ 14)
(defconstant +bio-ctrl-get-callback+ 15)
(defconstant +bio-c-set-buf-mem-eof-return+ 130)

(cffi:defcfun ("SSL_get_error" ssl-get-error) :int
  (ssl :pointer)
  (ret :int))
(cffi:defcfun ("SSL_shutdown" ssl-shutdown) :int
  (ssl :pointer))
(cffi:defcfun ("SSL_CTX_free" ssl-ctx-free) :void
  (ctx :pointer))
(cffi:defcfun ("SSL_free" ssl-free) :void
  (ssl :pointer))
(cffi:defcfun ("ERR_get_error" ssl-err-get-error) :int)
(cffi:defcfun ("ERR_error_string" ssl-err-error-string) :string
  (e :unsigned-long)
  (buf :pointer))
(cffi:defcfun ("ERR_reason_error_string" ssl-err-reason-error-string) :string
  (errcode :int))
(cffi:defcfun ("SSL_alert_type_string_long" ssl-alert-type-string-long) :string
  (err :int))
(cffi:defcfun ("SSL_alert_desc_string_long" ssl-alert-desc-string-long) :string
  (err :int))
(cffi:defcfun ("SSL_state_string_long" ssl-state-string-long) :string
  (ssl :pointer))
(cffi:defcfun ("TLSv1_method" ssl-tlsv1-method) :pointer)
(cffi:defcfun ("TLSv1_client_method" ssl-tlsv1-client-method) :pointer)
(cffi:defcfun ("TLSv1_server_method" ssl-tlsv1-server-method) :pointer)
(cffi:defcfun ("SSLv23_method" ssl-sslv23-method) :pointer)
(cffi:defcfun ("SSLv23_client_method" ssl-sslv23-client-method) :pointer)
(cffi:defcfun ("SSLv23_server_method" ssl-sslv23-server-method) :pointer)
(cffi:defcfun ("SSL_CTX_new" ssl-ctx-new) :pointer
  (method :pointer))
(cffi:defcfun ("SSL_CTX_ctrl" ssl-ctx-ctrl) :long
  (ctx :pointer)
  (cmd :int)
  (larg :unsigned-long)
  (parg :pointer))
(cffi:defcfun ("SSL_CTX_use_certificate_chain_file" ssl-ctx-use-certificate-chain-file) :int
  (ctx :pointer)
  (file :string))
(cffi:defcfun ("SSL_CTX_use_PrivateKey_file" ssl-ctx-use-privatekey-file) :int
  (ctx :pointer)
  (file :string)
  (type :int))
(cffi:defcfun ("SSL_CTX_use_RSAPrivateKey_file" ssl-ctx-use-rsaprivatekey-file) :int
  (ctx :pointer)
  (file :string)
  (type :int))
(cffi:defcfun ("SSL_CTX_set_default_verify_paths" ssl-ctx-set-default-verify-paths) :int
  (ctx :pointer))
(cffi:defcfun ("SSL_CTX_set_verify" ssl-ctx-set-verify) :int
  (ctx :pointer)
  (mode :int)
  (verify-callback :pointer))
(cffi:defcfun ("SSL_new" ssl-new) :pointer
  (ctx :pointer))
(cffi:defcfun ("BIO_new" ssl-bio-new) :pointer
  (type :pointer))
(cffi:defcfun ("SSL_set_cipher_list" ssl-set-cipher-list) :int
  (ssl :pointer)
  (ciphers :string))
(cffi:defcfun ("SSL_set_bio" ssl-set-bio) :int
  (ssl :pointer)
  (bio-read :pointer)
  (bio-write :pointer))
(cffi:defcfun ("SSL_set_info_callback" ssl-set-info-callback) :void
  (ssl :pointer)
  (callback :pointer))
(cffi:defcfun ("SSL_set_msg_callback" ssl-set-msg-callback) :void
  (ssl :pointer)
  (callback :pointer))
(cffi:defcfun ("SSL_state" ssl-state) :int
  (ssl :pointer))
(cffi:defcfun ("SSL_set_accept_state" ssl-set-accept-state) :void
  (ssl :pointer))
(cffi:defcfun ("SSL_set_connect_state" ssl-set-connect-state) :void
  (ssl :pointer))
(cffi:defcfun ("SSL_connect" ssl-connect) :int
  (ssl :pointer))
(cffi:defcfun ("SSL_accept" ssl-accept) :int
  (ssl :pointer))
(cffi:defcfun ("BIO_ctrl" ssl-bio-ctrl) :long
  (bio :pointer)
  (cmd :int)
  (arg :long)
  (parg :pointer))
(cffi:defcfun ("BIO_ctrl_pending" ssl-bio-ctrl-pending) :unsigned-int
  (bio :pointer))
(cffi:defcfun ("BIO_s_mem" ssl-bio-s-mem) :pointer)
(cffi:defcfun ("BIO_read" ssl-bio-read) :int
  (bio :pointer)
  (buf :pointer)
  (len :int))
(cffi:defcfun ("BIO_write" ssl-bio-write) :int
  (bio :pointer)
  (buf :pointer)
  (len :int))
(cffi:defcfun ("SSL_pending" ssl-pending) :int
  (ssl :pointer))
(cffi:defcfun ("SSL_read" ssl-read) :int
  (ssl :pointer)
  (buf :pointer)
  (len :int))
(cffi:defcfun ("SSL_write" ssl-write) :int
  (ssl :pointer)
  (buf :pointer)
  (len :int))

(defun & (&rest vals) (not (zerop (apply 'logand vals))))
(defun ssl-is-init-finished (ssl) (& (ssl-state ssl) +ssl-st-ok+))
(defun ssl-in-init (ssl) (& (ssl-state ssl) +ssl-st-init+))
(defun ssl-in-before (ssl) (& (ssl-state ssl) +ssl-st-before+))
(defun ssl-in-connect-init (ssl) (& (ssl-state ssl) +ssl-st-connect+))
(defun ssl-in-accept-init (ssl) (& (ssl-state ssl) +ssl-st-accept+))
(defun ssl-bio-set-mem-eof-return (bio v) (ssl-bio-ctrl bio +bio-c-set-buf-mem-eof-return+ v (cffi:null-pointer)))

(defun ssl-ctx-set-options (ctx options)
  "Function version of the openssl macro."
  (ssl-ctx-ctrl ctx +ssl-ctrl-options+ options (cffi:null-pointer)))
