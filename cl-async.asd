(asdf:defsystem cl-async-util
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.4.0"
  :description "Internal utilities for cl-async."
  :depends-on (#:cffi #:cl-libevent2 #:cl-ppcre)
  :components
  ((:file "util")))

(asdf:defsystem cl-async
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.4.0"
  :description "Asynchronous operations for Common Lisp."
  :depends-on (#:cffi #:trivial-features #:cl-libevent2 #:cl-async-util #:babel #:cl-ppcre #:trivial-gray-streams #:puri)
  :components
  ((:file "package")
   (:file "base" :depends-on ("package"))
   (:file "fd" :depends-on ("base"))
   (:file "timer" :depends-on ("base"))
   (:file "dns" :depends-on ("base"))
   (:file "tcp" :depends-on ("dns"))
   (:file "tcp-stream" :depends-on ("tcp"))
   (:file "http" :depends-on ("tcp"))
   (:file "signal" :depends-on ("base"))
   (:file "future" :depends-on ("base"))))

