(asdf:defsystem cl-async-ssl
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.3.0"
  :description "SSL Wrapper around cl-async socket implementation."
  :depends-on (#:cffi #:cl+ssl #:cl-libevent2 #:cl-libevent2-ssl #:cl-async)
  :components
  ((:file "tcp-ssl")))
