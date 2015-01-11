(in-package :cl-async-test)
(in-suite cl-async-test-core)

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
                        :event-cb (lambda (ev)
                                    (error ev))
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
  (let ((num-err 0)
        (caught-p nil))
    ;; FIXME: trying to connect to the unrouteable
    ;; address sometimes gives ETIMEDOUT and sometimes ECONNREFUSED
    (handler-case
      (async-let ()
        (test-timeout 2)
        (as:tcp-connect "1.24.3.4" 9090
          (lambda (sock data) (declare (ignore sock data)))
          :event-cb (lambda (ev)
                      (incf num-err)
                      (error ev))
          :data "hai"
          :read-timeout 1))
      (as:tcp-timeout ()
        (setf caught-p t))
      (as:tcp-refused ()
        (setf caught-p t)))
    (is-true caught-p)
    (is (= num-err 1))))

(test tcp-server-close
  "Make sure a tcp-server closes gracefully"
  (multiple-value-bind (closedp)
      (async-let ((closedp nil))
        (test-timeout 3)
        (let* ((server (as:tcp-server nil 41818
                         (lambda (sock data) (declare (ignore sock data)))
                         :event-cb (lambda (ev) (declare (ignore ev))))))
          (assert server () "failed to listen at port 41818")
          (as:tcp-connect "127.0.0.1" 41818
            (lambda (sock data) (declare (ignore sock data)))
            :event-cb (lambda (ev) (declare (ignore ev)))
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
          :event-cb (lambda (ev) (declare (ignore ev)))
          :stream t)
        (as:tcp-connect "127.0.0.1" 41818
          (lambda (sock data) (declare (ignore sock data)))
          :event-cb (lambda (ev) (declare (ignore ev)))
          :data "HELLO!"))
    (is (string= server-data "HELLO!"))))

(test test-address-in-use
  "Test SOCKET-ADDRESS-IN-USE error"
  (multiple-value-bind (first-successful-p)
    (async-let ((first-successful-p nil))
      (flet ((make-server ()
               (as:tcp-server nil 41818
                              (lambda (sock stream) (declare (ignore sock stream)))
                              :event-cb #'error)))
        (let ((server (make-server)))
          (setf first-successful-p t)
          (signals as:socket-address-in-use
            (make-server))
          (as:close-tcp-server server))))
    (is-true first-successful-p)))

(test no-overlap
  "Make sure that requests/responses don't overlap."
  (multiple-value-bind (res)
      (async-let ((res (make-hash-table :test 'eq)))
        (test-timeout 3)

        (let ((counter 1))
          (as:tcp-server nil 31389
            (lambda (sock data)
              (dotimes (i (length data))
                (assert (= (aref data 0) (aref data i))))
              (incf (getf (as:socket-data sock) :bytes) (length data))
              (when (<= (+ as:*buffer-size* 20000) (getf (as:socket-data sock) :bytes))
                (let ((res (make-array 500000 :initial-element (getf (as:socket-data sock) :id)
                                              :element-type 'as:octet)))
                  (as:write-socket-data sock res))))
            :connect-cb (lambda (sock)
                          (setf (as:socket-data sock) (list :id counter :bytes 0))
                          (incf counter))))
        (dotimes (i 4)
          (let ((x i))
            (as:tcp-connect "127.0.0.1" 31389
              (lambda (sock data)
                (declare (ignorable sock))
                (push data (gethash x res)))
              :data (make-array (+ as:*buffer-size* 20000)
                                :initial-element x
                                :element-type '(unsigned-byte 8))))))
    (loop ;for k being the hash-keys of res
          for v being the hash-values of res do
      (let ((stream (flexi-streams:make-in-memory-output-stream :element-type '(unsigned-byte 8))))
        (dolist (part v)
          (write-sequence part stream))
        (let ((bytes (flexi-streams:get-output-stream-sequence stream))
              (is-eq t))
          (dotimes (i (length bytes))
            (unless (= (aref bytes 0) (aref bytes i))
              (setf is-eq nil)
              (return)))
          (is (eq is-eq t)))))))
