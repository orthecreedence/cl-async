(asdf:defsystem cl-async-base
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.6.0"
  :description "Base system for cl-async."
  :depends-on (#:cffi #:cl-libuv #:bordeaux-threads)
  :components
  ((:file "base")))

(asdf:defsystem cl-async-util
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.6.0"
  :description "Internal utilities for cl-async."
  :depends-on (#:cffi
               #:fast-io
               #:cl-libuv
               #:cl-ppcre
               #:cl-async-base)
  :components
  ((:file "util")))

(asdf:defsystem cl-async
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.6.0"
  :description "Asynchronous operations for Common Lisp."
  :depends-on (#:cffi
               #:trivial-features
               #:static-vectors
               #:cl-libuv
               #:cl-async-base
               #:cl-async-util
               #:cl-async-future
               #:babel
               #:cl-ppcre
               #:trivial-gray-streams)
  :components
  ((:file "package")
   (:file "event-loop" :depends-on ("package"))
   (:file "event" :depends-on ("package"))
   (:file "dns" :depends-on ("package"))
   (:file "tcp" :depends-on ("dns"))
   (:file "tcp-stream" :depends-on ("tcp"))
   (:file "signal" :depends-on ("package"))
   (:file "notify" :depends-on ("package"))))

