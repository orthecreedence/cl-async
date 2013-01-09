(in-package :cl-async)

(defclass async-stream (trivial-gray-stream-mixin)
  ((socket :accessor stream-socket :initarg :socket :initform nil))
  (:documentation "The underlying class for async streams. Wraps a tcp socket class."))
(defclass async-output-stream (async-stream fundamental-binary-output-stream) ()
  (:documentation "Async output stream."))
(defclass async-input-stream (async-stream fundamental-binary-input-stream) ()
  (:documentation "Async input stream."))
(defclass async-io-stream (async-input-stream async-output-stream) ()
  (:documentation "Async stream for both input and output."))

;; -----------------------------------------------------------------------------
;; base stream
;; -----------------------------------------------------------------------------
(defmethod stream-output-type ((stream async-stream))
  "This is always a binary stream."
  '(unsigned-byte 8))

(defmethod stream-element-type ((stream async-stream))
  "This is always a binary stream."
  '(unsigned-byte 8))

(defmethod open-stream-p ((stream async-stream))
  "Test the underlying socket to see if this stream is open."
  (let ((socket (stream-socket stream)))
    (not (as:socket-closed-p socket))))

(defmethod close ((stream async-stream) &key abort)
  "Close the stream. If aborting, attempt to clear out remaining data in the
   buffers before closing (is this really needed?)"
  (when abort
    (when (output-stream-p stream)
      (clear-output stream))
    (when (input-stream-p stream)
      (clear-input stream)))
  (as:close-socket (stream-socket stream)))

;; -----------------------------------------------------------------------------
;; output stream
;; -----------------------------------------------------------------------------
(defmethod stream-clear-output ((stream async-output-stream))
  "Attempt to clear the output buffer of an output stream."
  (when (open-stream-p stream)
    (let* ((socket (stream-socket stream))
           (bev (socket-c socket))
           (bev-output (le::bufferevent-get-output bev)))
      (loop while (< 0 (le:evbuffer-get-length bev-output)) do
        (le:evbuffer-drain bev-output 9999999)))))

(defmethod stream-force-output ((stream async-output-stream))
  "Force an output stream to send its data to the underlying fd."
  (when (open-stream-p stream)
    (let* ((socket (stream-socket stream))
           (bev (socket-c socket))
           (bev-output (le:bufferevent-get-output bev))
           (fd (le:bufferevent-getfd bev)))
      (le:evbuffer-write bev-output fd))))

(defmethod stream-finish-output ((stream async-output-stream))
  "Really, since we're async, same as force-output."
  (stream-force-output stream))

(defmethod stream-write-byte ((stream async-output-stream) byte)
  "Write one byte to the underlying socket."
  (when (open-stream-p stream)
    (write-socket-data (stream-socket stream) (vector byte))))
  
(defmethod stream-write-sequence ((stream async-output-stream) sequence start end &key)
  "Write a sequence of bytes to the underlying socket."
  (when (open-stream-p stream)
    (let ((seq (subseq sequence start end)))
      (write-socket-data (stream-socket stream) seq))))

;; -----------------------------------------------------------------------------
;; input stream
;; -----------------------------------------------------------------------------
(defmethod stream-clear-input ((stream async-input-stream))
  "Attempt to clear the input buffer of an input stream."
  (when (open-stream-p stream)
    (let* ((socket (stream-socket stream))
           (bev (socket-c socket))
           (bev-input (le::bufferevent-get-input bev)))
      (loop while (< 0 (le:evbuffer-get-length bev-input)) do
        (le:evbuffer-drain bev-input 9999999)))))

(defmethod stream-read-byte ((stream async-input-stream))
  "Read one byte from the underlying socket."
  (let ((byte (read-bytes-from-socket (stream-socket stream) 1)))
    (if byte
        (aref byte 0)
        :eof)))

(defmethod stream-read-sequence ((stream async-input-stream) sequence start end &key)
  "Attempt to read a sequence of bytes from the underlying socket."
  (let ((seq (read-bytes-from-socket (stream-socket stream) (- end start))))
    (if seq
        (progn
          (replace sequence seq)
          (length seq))
        start)))

