(asdf:defsystem cl-async-ssl
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.6.0"
  :description "SSL Wrapper around cl-async socket implementation."
  :depends-on (#:cffi
               #:cl-async
               #:vom)
  :components
  ((:file "ssl/package")
   (:file "ssl/util" :depends-on ("ssl/package"))
   (:file "ssl/tcp" :depends-on ("ssl/package" "ssl/util"))))
