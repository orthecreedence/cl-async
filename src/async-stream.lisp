(in-package :cl-async)

(defclass async-stream (trivial-gray-stream-mixin)
  ((streamish :accessor streamish :initarg :streamish :initarg :socket :initform nil)
   (buffer :accessor stream-buffer :initform (make-buffer)))
  (:documentation "The underlying class for async streams. Wraps a streamish."))
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
  (write-to-buffer bytes (stream-buffer stream)))

(defmethod stream-output-type ((stream async-stream))
  "This is always a binary stream."
  'cl-async-util:octet)

(defmethod stream-element-type ((stream async-stream))
  "This is always a binary stream."
  'cl-async-util:octet)

(defmethod open-stream-p ((stream async-stream))
  "Test the underlying streamish to see if this stream is open."
  (not (streamish-closed-p (streamish stream))))

(defmethod close ((stream async-stream) &key abort)
  "Close the stream. If aborting, attempt to clear out remaining data in the
   buffers before closing (is this really needed?)"
  (when abort
    (when (output-stream-p stream)
      (clear-output stream))
    (when (input-stream-p stream)
      (clear-input stream)))
  (close-streamish (streamish stream)))

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

(defmethod stream-write-sequence ((stream async-output-stream) sequence start end &key)
  "Write a sequence of bytes to the underlying streamish."
  (when (open-stream-p stream)
    (let ((seq (subseq sequence start end)))
      (streamish-write (streamish stream) seq)
      seq)))

(defmethod stream-write-byte ((stream async-output-stream) byte)
  "Write one byte to the underlying streamish."
  (stream-write-sequence stream (make-array 1 :element-type 'octet
                                              :initial-element byte) 0 1))

(defmethod send-buffered-data ((stream async-output-stream))
  "Take data we've buffered between initial sending and actual streamish
   and send it out."
  (let ((data (buffer-output (stream-buffer stream))))
    (setf (stream-buffer stream) (make-buffer))
    (streamish-write (streamish stream) data))
  nil)

;; -----------------------------------------------------------------------------
;; input stream
;; -----------------------------------------------------------------------------
(defmethod stream-clear-input ((stream async-input-stream))
  "Attempt to clear the input buffer of an input stream."
  (when (open-stream-p stream)
    (setf (stream-buffer stream) (make-buffer))))

(defmethod stream-read-byte ((stream async-input-stream))
  "Read one byte from the underlying streamish."
  (let* ((buff (make-array 1 :element-type '(unsigned-byte 8)))
         (num (stream-read-sequence stream buff 0 1)))
    (if (= num 1)
        (aref buff 0)
        :eof)))

(defmethod stream-read-sequence ((stream async-input-stream) sequence start end &key)
  "Attempt to read a sequence of bytes from the underlying streamish."
  (let* ((buffer (buffer-output (stream-buffer stream)))
         (numbytes (min (length buffer) (- end start))))
    (setf (stream-buffer stream) (make-buffer (subseq buffer numbytes)))
    (replace sequence buffer :start1 start :end1 end)
    numbytes))

;;;; compatibility

(defun stream-socket (stream)
  (streamish stream))
