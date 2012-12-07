(asdf:defsystem cl-async-test
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1"
  :description "TESTS FOR Asynchronous operations for Common Lisp."
  :depends-on (#:cffi #:cl-async #:eos #:bordeaux-threads)
  :components
  ((:module test
	:components ((:file "util")
	             (:file "base" :depends-on ("util"))
	             (:file "run")
		         (:file "timer" :depends-on ("base"))
		         (:file "dns" :depends-on ("base"))
		         (:file "tcp" :depends-on ("base"))
		         (:file "tcp-stream" :depends-on ("tcp"))
		         (:file "http" :depends-on ("base"))
		         (:file "signal" :depends-on ("base"))
		         (:file "future" :depends-on ("base"))))))

