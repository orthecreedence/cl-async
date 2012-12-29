(asdf:defsystem cl-async-test
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1"
  :description "TESTS FOR Asynchronous operations for Common Lisp."
  :depends-on (#:cffi #:cl-async #:eos #:bordeaux-threads #:usocket)
  :components
  ((:module test
    :serial t
	:components ((:file "util")
	             (:file "base")
		         (:file "timer")
		         (:file "dns")
		         (:file "tcp")
		         (:file "tcp-stream")
		         (:file "http")
		         (:file "signal")
		         (:file "future")
	             (:file "run")))))

