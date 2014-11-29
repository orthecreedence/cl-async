(in-package :cl-async-test)
(def-suite cl-async-ssl-test :in cl-async-test
                             :description "cl-async ssl test suite")
(in-suite cl-async-ssl-test)

(test tcp-ssl-simple-client-server
  "Test a simple client/server implementation over SSL"
  (multiple-value-bind (server-reqs server-data connect-num client-replies client-data)
      (async-let ((server-reqs 0)
                  (server-data "")
                  (connect-num 0)
                  (client-replies 0)
                  (client-data ""))
        (test-timeout 3)

        (as-ssl:tcp-ssl-server nil 31389
          (lambda (sock data)
            (incf server-reqs)
            (setf server-data (concat server-data (babel:octets-to-string data)))
            (as:write-socket-data sock "thxlol "))
          nil
          :certificate (asdf:system-relative-pathname :cl-async #P"test/ssl/certkey")
          :key  (asdf:system-relative-pathname :cl-async #P"test/ssl/certkey")
          :connect-cb (lambda (sock)
                        (declare (ignore sock))
                        (incf connect-num)))

        (as:delay
          (lambda ()
            (dolist (addr '("127.0.0.1" "localhost"))
              ;; split request "hai " up between tcp-ssl-connect and write-socket-data
              (let* ((sock (as-ssl:tcp-ssl-connect addr 31389
                             (lambda (sock data)
                               (incf client-replies)
                               (unless (as:socket-closed-p sock)
                                 (as:close-socket sock))
                               (setf client-data (concat client-data (babel:octets-to-string data))))
                             (lambda (ev) (error ev))
                             :data "ha")))
                (as:write-socket-data sock "i "))))
          :time .2)

        (as:delay (lambda () (as:exit-event-loop))
                  :time 2))
    (is (= server-reqs 2) "number of server requests: ~s != ~s" server-reqs 2)
    (is (string= server-data "hai hai ") "received server data: ~s != ~s" server-data "hai hai ")
    (is (= connect-num 2) "number of connections (from connect-cb): ~s != ~s" connect-num 2)
    (is (= client-replies 2) "number of replies sent to client: ~s != ~s" client-replies 2)
    (is (string= client-data "thxlol thxlol ") "received client data: ~s != ~s" client-data "thxlol thxlol ")))

