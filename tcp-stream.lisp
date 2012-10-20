(ql:quickload :cl-async)
(ql:quickload :trivial-gray-streams)
(ql:quickload :chunga)
(ql:quickload :flexi-streams)
(ql:quickload :drakma)

(defpackage :stream-test
  (:use :cl :trivial-gray-streams :chunga :flexi-streams))
(in-package :stream-test)

(defclass async-stream (trivial-gray-stream-mixin)
  ((socket :accessor stream-socket :initarg :socket :initform nil)))
(defclass async-output-stream (fundamental-binary-output-stream async-stream) ())
(defclass async-input-stream (fundamental-binary-input-stream async-stream) ())
(defclass async-io-stream (async-input-stream async-output-stream) ())

;; base stream
;; -----------------------------------------------------------------------------
(defmethod stream-output-type ((stream async-stream))
  '(unsigned-byte 8))

(defmethod open-stream-p ((stream async-stream))
  (let ((socket (stream-socket stream)))
    (and (subtypep (type-of socket) 'as:socket)
         (not (as:socket-closed-p socket)))))

(defmethod close ((stream async-stream) &key abort)
  (format t "CLOSE STREAM: abort: ~a~%" abort))

;; output stream
;; -----------------------------------------------------------------------------
(defmethod stream-clear-output ((stream async-output-stream)) t)
(defmethod stream-finish-output ((stream async-output-stream)) t)
(defmethod stream-force-output ((stream async-output-stream)) t)

(defmethod stream-write-byte ((stream async-output-stream) byte)
  (when (open-stream-p stream)
    (format t "WRITE BYTE: ~s~%" (code-char byte))))
  
(defmethod stream-write-sequence ((stream async-stream) sequence start end &key)
  (format t "WRITE SEQ: ~a -> ~a~%~a~%------~%" start end (babel:octets-to-string sequence :encoding :utf-8)))

;; input stream
;; -----------------------------------------------------------------------------
(defmethod stream-clear-input ((stream async-input-stream)) t)
(defmethod stream-read-byte ((stream async-input-stream))
  (format t "READ BYTE~%"))
(defmethod stream-read-sequence ((stream async-input-stream) sequence start end &key)
  (format t "READ SEQ: ~a -> ~a~%" start end))

(defparameter *as-stream* (make-instance 'async-io-stream :socket (make-instance 'as:socket)))
(defparameter *fl-stream* (make-flexi-stream (make-chunked-stream *as-stream*)))

(drakma:http-request "http://api.musio.com/" :stream *fl-stream*)
