(asdf:defsystem cl-async
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.2.6"
  :description "Asynchronous operations for Common Lisp."
  :depends-on (#:cffi #:cl-libevent2 #:babel #:cl-ppcre #:trivial-gray-streams #:puri)
  :components
  ((:file "package")
   (:file "util" :depends-on ("package"))
   (:file "common" :depends-on ("util"))
   (:file "timer" :depends-on ("common"))
   (:file "dns" :depends-on ("common"))
   (:file "tcp" :depends-on ("dns"))
   (:file "tcp-stream" :depends-on ("tcp"))
   (:file "http" :depends-on ("tcp"))
   (:file "signal" :depends-on ("common"))
   (:file "future" :depends-on ("common"))))

