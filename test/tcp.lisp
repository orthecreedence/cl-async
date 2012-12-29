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
        (test-timeout 3)

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
          ;; split request "hai " up between tcp-connect and write-socket-data
          (let ((sock (as:tcp-connect addr 31388
                        (lambda (sock data)
                          (incf client-replies)
                          (unless (as:socket-closed-p sock)
                            (as:close-socket sock))
                          (setf client-data (concat client-data (babel:octets-to-string data))))
                        (lambda (ev) (error ev))
                        :data "ha")))
            (as:write-socket-data sock "i ")))

        (as:delay (lambda () (as:exit-event-loop))
                  :time 1))
    (is (= server-reqs 2) "number of server requests")
    (is (string= server-data "hai hai ") "received server data")
    (is (= connect-num 2) "number of connections (from connect-cb)")
    (is (= client-replies 2) "number of replies sent to client")
    (is (string= client-data "thxlol thxlol ") "received client data")))

(test tcp-connect-fail
  "Make sure a tcp connection fails"
  (let ((num-err 0))
    (signals as:tcp-timeout
      (async-let ()
        (test-timeout 2)
        (as:tcp-connect "1.24.3.4" 3
          (lambda (sock data) (declare (ignore sock data)))
          (lambda (ev)
            (incf num-err)
            (error ev))
          :data "hai"
          :read-timeout 1)))
    (is (= num-err 1))))

(test tcp-server-close
  "Make sure a tcp-server closes gracefully"
  (multiple-value-bind (closedp)
      (async-let ((closedp nil))
        (test-timeout 3)
        (let* ((server (as:tcp-server nil 41818
                         (lambda (sock data) (declare (ignore sock data)))
                         (lambda (ev) (declare (ignore ev))))))
          (as:tcp-connect "127.0.0.1" 41818
            (lambda (sock data) (declare (ignore sock data)))
            (lambda (ev) (declare (ignore ev)))
            :connect-cb
              (lambda (sock)
                (as:delay
                  (lambda ()
                    (let ((closed-pre (as::tcp-server-closed server)))
                      (as:close-socket sock)
                      (as:delay
                        (lambda ()
                          (setf closedp (and closed-pre
                                             (as::tcp-server-closed server)))))))
                  :time 1)))
          (as:delay (lambda () (as:close-tcp-server server)) :time .1)))
    (is-true closedp)))

(test tcp-server-stream
  "Make sure a tcp-server stream functions properly"
  (multiple-value-bind (server-data)
      (async-let ((server-data nil))
        (test-timeout 3)
        (as:tcp-server nil 41818
          (lambda (sock stream)
            (let ((buff (make-array 1024 :element-type '(unsigned-byte 8))))
              (loop for n = (read-sequence buff stream)
                    while (< 0 n) do
                (setf server-data (concat server-data (babel:octets-to-string (subseq buff 0 n))))))
            (as:close-socket sock)
            (as:exit-event-loop))
          (lambda (ev) (declare (ignore ev)))
          :stream t)
        (as:tcp-connect "127.0.0.1" 41818
          (lambda (sock data) (declare (ignore sock data)))
          (lambda (ev) (declare (ignore ev)))
          :data "HELLO!"))
    (is (string= server-data "HELLO!"))))
                          
(defun get-usocket-fd (connection)
  "Gets the fd from a usocket connection."
  (let* ((type (type-of connection))
         (stream (cond ((subtypep type 'usocket:stream-usocket)
                        (usocket:socket-stream connection))
                       ((subtypep type 'stream)
                        connection))))
    (when stream
      #+sbcl
        (sb-sys:fd-stream-fd stream)
      #+cmu
        (system:fd-stream-fd stream)
      #+ccl
        (ccl::ioblock-device (ccl::stream-ioblock stream t))
      #+clisp
        (ext:stream-handles stream))))

(test wrap-existing-fd
  "Make sure wrapping an existing file descriptor works"
  (multiple-value-bind (response)
      (async-let ((response nil))
        (test-timeout 3)
        (let* ((server (as:tcp-server nil 31311
                         (lambda (sock data)
                           (as:write-socket-data sock (concat (babel:octets-to-string data) " lol")))
                         (lambda (ev) (declare (ignore ev))))))
          (as:delay
            (lambda ()
              (let* ((conn (usocket:socket-connect "127.0.0.1" 31311))
                     (sock (as:init-tcp-socket
                             (lambda (sock data)
                               (setf response (babel:octets-to-string data))
                               (as:close-socket sock)
                               (as:close-tcp-server server))
                             (lambda (ev)
                               (format t "EV: ~a~%" ev))
                             :fd (get-usocket-fd conn))))
                (as:write-socket-data sock "omg")))
            :time .2)))
    (is (string= response "omg lol"))))

