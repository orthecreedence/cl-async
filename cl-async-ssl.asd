(asdf:defsystem cl-async-ssl
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.2.6"
  :description "SSL Wrapper around cl-async socket implementation."
  :depends-on (#:cffi #:cl+ssl #:libevent2 #:cl-async)
  :components
  ((:file "ssl")))
