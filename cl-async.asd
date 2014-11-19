(asdf:defsystem cl-async-base
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.5.3"
  :description "Base system for cl-async."
  :depends-on (#:cffi #:cl-libevent2 #:bordeaux-threads)
  :components
  ((:file "base")))

(asdf:defsystem cl-async-util
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.5.3"
  :description "Internal utilities for cl-async."
  :depends-on (#:cffi #:cl-libevent2 #:cl-ppcre #:cl-async-base)
  :components
  ((:file "util")))

(asdf:defsystem cl-async
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.5.3"
  :description "Asynchronous operations for Common Lisp."
  :depends-on (#:cffi #:trivial-features #:cl-libevent2 #:cl-async-base #:cl-async-util #:babel #:cl-ppcre #:trivial-gray-streams #:puri)
  :components
  ((:file "package")
   (:file "event-loop" :depends-on ("package"))
   (:file "event" :depends-on ("package"))
   (:file "dns" :depends-on ("package"))
   (:file "tcp" :depends-on ("dns"))
   (:file "tcp-stream" :depends-on ("tcp"))
   (:file "signal" :depends-on ("package"))))

