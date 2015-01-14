(in-package :cl-async-test)
(in-suite cl-async-test-core)

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

(test poll-fd
  "Make sure polling a file descriptor works."
  (multiple-value-bind (response)
      (async-let ((response nil))
        (test-timeout 5)
        (let (server)
          (setf server (as:tcp-server nil 31311
                         (lambda (sock data)
                           (as:write-socket-data sock (concat (babel:octets-to-string data) " lol"))
                           (as:close-tcp-server server))
                         :event-cb (lambda (ev) (declare (ignore ev)))))
          (as:with-delay (.1)
            (let* ((sock (usocket:socket-connect "127.0.0.1" 31311 :element-type 'octet))
                   (stream (usocket:socket-stream sock))
                   (fd (get-usocket-fd sock))
                   (poller nil)
                   (poll-cb (lambda (events)
                              (declare (ignore events))
                              (as:with-delay ()
                                (usocket:socket-close sock)
                                (as:free-poller poller))
                              (when (listen stream)
                                (let* ((buff (make-array 7 :element-type 'octet))
                                       (len (read-sequence buff stream)))
                                  (setf response (babel:octets-to-string (subseq buff 0 len))))))))
              (setf poller (as:poll fd poll-cb :poll-for '(:readable) :socket t))
              (write-sequence (babel:string-to-octets "omg") stream)
              (force-output stream)))))
    (is (string= response "omg lol"))))
