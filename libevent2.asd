(asdf:defsystem libevent2
  :depends-on (#:cffi)
  :components
  ((:module libevent2
	        :components ((:file "libevent2")
			             (:file "wrapper" :depends-on ("libevent2"))
						 (:file "bindings" :depends-on ("libevent2" "wrapper"))
						 ;(:file "exports" :depends-on ("libevent2" "wrapper" "bindings"))
						 ;(:file "accessors" :depends-on ("bindings" "exports"))
                         ))))
