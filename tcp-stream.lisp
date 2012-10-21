(in-package :cl-async)

(defclass async-stream (trivial-gray-stream-mixin)
  ((socket :accessor stream-socket :initarg :socket :initform nil)))
(defclass async-output-stream (fundamental-binary-output-stream async-stream) ())
(defclass async-input-stream (fundamental-binary-input-stream async-stream) ())
(defclass async-io-stream (async-input-stream async-output-stream) ())

;; -----------------------------------------------------------------------------
;; base stream
;; -----------------------------------------------------------------------------
(defmethod stream-output-type ((stream async-stream))
  '(unsigned-byte 8))

(defmethod open-stream-p ((stream async-stream))
  (let ((socket (stream-socket stream)))
    (not (as:socket-closed-p socket))))

(defmethod close ((stream async-stream) &key abort)
  (declare (ignore abort))
  (as:close-socket (stream-socket stream)))

;; -----------------------------------------------------------------------------
;; output stream
;; -----------------------------------------------------------------------------
(defmethod stream-clear-output ((stream async-output-stream))
  (when (open-stream-p stream)
    (let* ((socket (stream-socket stream))
           (bev (socket-c socket))
           (bev-output (le::bufferevent-get-output bev)))
      (loop while (< 0 (le:evbuffer-get-length bev-output)) do
        (le:evbuffer-drain bev-output 9999999)))))

(defmethod stream-force-output ((stream async-output-stream))
  (when (open-stream-p stream)
    (let* ((socket (stream-socket stream))
           (bev (socket-c socket))
           (bev-output (le:bufferevent-get-output bev))
           (fd (le:bufferevent-getfd bev)))
      (le:evbuffer-write bev-output fd))))

(defmethod stream-finish-output ((stream async-output-stream))
  (stream-force-output stream))

(defmethod stream-write-byte ((stream async-output-stream) byte)
  (when (open-stream-p stream)
    (write-socket-data (stream-socket stream) (vector byte))))
  
(defmethod stream-write-sequence ((stream async-stream) sequence start end &key)
  (when (open-stream-p stream)
    (let ((seq (subseq sequence start end)))
      (format t "seq: ~a~%" seq)
      (write-socket-data (stream-socket stream) seq))))

;; -----------------------------------------------------------------------------
;; input stream
;; -----------------------------------------------------------------------------
(defmethod stream-clear-input ((stream async-input-stream))
  (when (open-stream-p stream)
    (let* ((socket (stream-socket stream))
           (bev (socket-c socket))
           (bev-input (le::bufferevent-get-input bev)))
      (loop while (< 0 (le:evbuffer-get-length bev-input)) do
        (le:evbuffer-drain bev-input 9999999)))))

(defmethod stream-read-byte ((stream async-input-stream))
  (let ((byte (as::read-bytes-from-socket (stream-socket stream) 1)))
    (if byte
        (aref byte 0)
        :eof)))

(defmethod stream-read-sequence ((stream async-input-stream) sequence start end &key)
  (let ((seq (as::read-bytes-from-socket (stream-socket stream) (- end start))))
    (if seq
        (progn
          (replace sequence seq)
          end)
        start)))

;(defparameter *as-stream* (make-instance 'async-io-stream :socket (make-instance 'as:socket)))
;(defparameter *fl-stream* (make-flexi-stream (make-chunked-stream *as-stream*)))

;(drakma:http-request "http://api.musio.com/" :stream *fl-stream*)
