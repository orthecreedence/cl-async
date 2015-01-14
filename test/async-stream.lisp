(in-package :cl-async-test)
(in-suite cl-async-test-core)

;; TODO: read/write-byte

(defparameter *stream-buffer* (make-array 8096 :element-type '(unsigned-byte 8)))

(test stream-read-write-sequence
  "Test both tcp stream read-sequence and write-sequence"
  (multiple-value-bind (server-data client-data)
      (async-let ((server-data nil)
                  (client-data nil))
        ;(test-timeout 3)

        (let ((server (as:tcp-server nil 31388
                        (lambda (sock data)
                          (setf server-data (babel:octets-to-string data))
                          ;; for good measure, test writing to a new stream from server
                          (write-sequence (babel:string-to-octets "don't say that")
                                          (make-instance 'as:async-io-stream :socket sock))))))
          ;; launch a client, which will write its data to a stream
          (as:with-delay ()
            (let ((stream (as:tcp-connect "127.0.0.1" 31388
                            (lambda (sock stream)
                              ;; got a response, read from the stream
                              (let ((num-bytes (read-sequence *stream-buffer* stream)))
                                (setf client-data (babel:octets-to-string (subseq *stream-buffer* 0 num-bytes)))
                                (as:close-socket sock)
                                (as:close-tcp-server server)))
                            :stream t)))
              (write-sequence (babel:string-to-octets "can i have your coat?") stream)))))
    (is (string= server-data "can i have your coat?"))
    (is (string= client-data "don't say that"))))
