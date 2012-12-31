;;; DEPRECATED. these tests should work with the current version of cl-async's
;;; HTTP implementation, but will no longer be updated. Use drakma-async to
;;; replace http-client.

(in-package :cl-async-test)
(in-suite cl-async-test-core)

;; TODO: timeouts (integer, float)

(test http-simple-client-server
  "Test both http-client and http-server"
  (multiple-value-bind (server-reqs server-data client-replies client-data)
      (async-let ((server-reqs 0)
                  (server-data "")
                  (client-replies 0)
                  (client-data ""))
        (test-timeout 2)

        (as:http-server nil 31388
          (lambda (req)
            (incf server-reqs)
            (let ((body (or (as:http-request-body req) (make-array 0 :element-type '(unsigned-byte 8)))))
              (setf server-data (concat server-data (babel:octets-to-string body))))
            (as:http-response req :body "thxlol "))
          nil)

        (dolist (addr '("http://127.0.0.1:31388/" "http://localhost:31388/"))
          ;; split request "hai " up between http-client and write-socket-data
          (as:http-client addr
            (lambda (status headers body)
              (declare (ignore status headers))
              (incf client-replies)
              (setf client-data (concat client-data (babel:octets-to-string body))))
            (lambda (ev) (error ev))
            ;; NOTE: disabled until i figure out how to force http-server to
            ;; read the body of GET requests. in other words, this test SHOULD
            ;; fail until the server is fixed.
            ;:method :post
            :body "hai "))

        (as:delay (lambda () (as:exit-event-loop))
                  :time 1))
    (is (= server-reqs 2) "number of server requests")
    (is (string= server-data "hai hai ") "Wrong data recieved by server (known bug: https://github.com/orthecreedence/cl-async/issues/39)")
    (is (= client-replies 2) "number of replies sent to client")
    (is (string= client-data "thxlol thxlol ") "received client data")))

(test http-connect-fail
  "Make sure a http connection fails"
  (let ((num-err 0))
    (signals as:http-refused
      (async-let ()
        (test-timeout 2)
        (as:http-client "http://1.24.3.4:3"
          (lambda (status headers body) (declare (ignore status headers body)))
          (lambda (ev)
            (incf num-err)
            (error ev))
          :body "hai"
          :timeout 1)))
    (is (= num-err 1))))

(test http-server-close
  "Make sure a http-server closes gracefully"
  (multiple-value-bind (closedp)
      (async-let ((closedp nil))
        (test-timeout 3)
        (let* ((server (as:http-server nil 41818
                         (lambda (req)
                           (as:http-response req :body "lol"))
                         (lambda (ev) (declare (ignore ev))))))
          (as:http-client "http://127.0.0.1:41818"
            (lambda (status headers body)
              (declare (ignore status headers body))
              (as:delay
                (lambda ()
                  (let ((closed-pre (as::http-server-closed server)))
                    (as:delay
                      (lambda ()
                        (setf closedp (and closed-pre
                                           (as::http-server-closed server)))))))
                :time 1))
            (lambda (ev) (declare (ignore ev))))
          (as:delay (lambda () (as:close-http-server server)) :time .1)))
    (is-true closedp)))

