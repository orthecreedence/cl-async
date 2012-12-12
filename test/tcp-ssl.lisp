(in-package :cl-async-test)
(in-suite cl-async-test)

;; TODO: somehow tie these tests in with cl-async-ssl
;; TODO: once SSL is implemented in the server, test server implementation

#|
(test tcp-ssl-client
  "Make sure an SSL client works (requires internet access)"
  (multiple-value-bind (headers)
      (async-let ((response nil))
        (test-timeout 5)

        (let* ((sock (as:tcp-connect "www.google.com" 443
                       (lambda (sock data)
                         (setf response data)
                         (as:close-socket sock)
                         nil)))
               (sock-ssl (as-ssl:wrap-in-ssl sock)))
          (write-socket-data sock-ssl (format nil "GET /~c~c" #\return #\newline))))
    (is (< 0 (length response)))))
|#
