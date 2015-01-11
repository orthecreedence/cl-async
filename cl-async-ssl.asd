(asdf:defsystem cl-async-ssl
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.6.0"
  :description "SSL Wrapper around cl-async socket implementation."
  :depends-on (#:cffi
               #:cl-async
               #:vom)
  :components
  ((:file "src/ssl/package")
   (:file "src/ssl/util" :depends-on ("src/ssl/package"))
   (:file "src/ssl/tcp" :depends-on ("src/ssl/package" "src/ssl/util"))))
