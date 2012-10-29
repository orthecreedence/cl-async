(asdf:defsystem cl-async
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.2.4"
  :description "Asynchronous operations for Common Lisp."
  :depends-on (#:cffi #:libevent2 #:babel #:cl-ppcre #:puri)
  :components
  ((:file "package")
   (:file "util" :depends-on ("package"))
   (:file "common" :depends-on ("util"))
   (:file "timer" :depends-on ("common"))
   (:file "dns" :depends-on ("common"))
   (:file "tcp" :depends-on ("dns"))
   (:file "http" :depends-on ("tcp"))
   (:file "signal" :depends-on ("common"))))
