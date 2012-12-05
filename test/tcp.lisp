(in-package :cl-async-test)
(in-suite cl-async-test)

;; TODO: timeouts (integer, float)

(test tcp-simple-client-server
  "Test both tcp-connect and tcp-server"
  (multiple-value-bind (server-reqs server-data connect-num client-replies client-data)
      (async-let ((server-reqs 0)
                  (server-data "")
                  (connect-num 0)
                  (client-replies 0)
                  (client-data ""))

        (as:tcp-server nil 31388
          (lambda (sock data)
            (incf server-reqs)
            (setf server-data (concat server-data (babel:octets-to-string data)))
            (as:write-socket-data sock "thxlol "))
          nil
          :connect-cb (lambda (sock)
                        (declare (ignore sock))
                        (incf connect-num)))

        (dolist (addr '("127.0.0.1" "localhost"))
          (as:tcp-connect addr 31388
            (lambda (sock data)
              (incf client-replies)
              (unless (as:socket-closed-p sock)
                (as:close-socket sock))
              (setf client-data (concat client-data (babel:octets-to-string data))))
            (lambda (ev) (error ev))
            :data "hai "))

        (as:delay (lambda () (as:exit-event-loop))
                  :time 2))
    (is (= server-reqs 2) "number of server requests")
    (is (string= server-data "hai hai ") "received server data")
    (is (= connect-num 2) "number of connections (from connect-cb)")
    (is (= client-replies 2) "number of replies sent to client")
    (is (string= client-data "thxlol thxlol ") "received client data")))

(test tcp-connect-fail
  "Make sure a tcp connection fails"
  (signals as:tcp-timeout
    (async-let ()
      (as:tcp-connect "1.24.3.4" 3
        (lambda (sock data) (declare (ignore sock data)))
        (lambda (ev) (error ev))
        :data "hai"
        :read-timeout 1))))

