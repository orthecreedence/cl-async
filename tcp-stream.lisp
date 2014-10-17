(in-package :cl-async)

(defun make-buffer (&optional data)
  (make-array 0 :element-type 'octet :initial-contents data))

(defclass async-stream (trivial-gray-stream-mixin)
  ((socket :accessor stream-socket :initarg :socket :initform nil)
   (buffer :accessor stream-buffer :initform (make-buffer)))
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
(defmethod stream-append-bytes ((stream async-stream) bytes)
  "Append some data to a stream's underlying buffer."
  (setf (stream-buffer stream) (cl-async-util:append-array (stream-buffer stream) bytes)))

(defmethod stream-output-type ((stream async-stream))
  "This is always a binary stream."
  'cl-async-util:octet)

(defmethod stream-element-type ((stream async-stream))
  "This is always a binary stream."
  'cl-async-util:octet)

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
  ;; we don't have the concept of an output buffer, only an underlying UV stream
  ;; that's either written to or isn't
  nil)

(defmethod stream-force-output ((stream async-output-stream))
  "Force an output stream to send its data to the underlying fd."
  ;; we don't have the concept of an output buffer, only an underlying UV stream
  ;; that's either written to or isn't
  nil)

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
    (setf (stream-buffer stream) (make-empty-buffer))))

(defmethod stream-read-byte ((stream async-input-stream))
  "Read one byte from the underlying socket."
  (let* ((buff (make-array 1 :element-type '(unsigned-byte 8)))
         (num (stream-read-sequence stream buff 0 1)))
    (if (= num 1)
        (aref buff 0)
        :eof)))

(defmethod stream-read-sequence ((stream async-input-stream) sequence start end &key)
  "Attempt to read a sequence of bytes from the underlying socket."
  (let* ((numbytes (- end start))
         (buffer (stream-buffer stream))
         (bytes (subseq buffer start (min (length buffer) numbytes))))
    (setf (stream-buffer stream) (make-buffer (subseq buffer numbytes)))
    (replace sequence bytes)
    (length bytes)))

