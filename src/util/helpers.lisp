(in-package :cl-async-util)

(deftype octet () '(unsigned-byte 8))
(deftype octet-vector () '(simple-array octet (*)))
(deftype bytes-or-string () '(or octet-vector string))
(deftype callback () '(or null function symbol))

(defun bytes (vector)
  "Convert any vector/string into a byte array. Useful for sending direct byte
   data into write-socket-data."
  (coerce vector '(vector octet)))

(defun make-buffer (&optional data)
  "Create an octet buffer, optoinally filled with the given data."
  (declare (type (or null octet-vector) data))
  (let ((buffer (fast-io:make-output-buffer)))
    (when data
      (write-to-buffer data buffer))
    buffer))

(declaim (inline buffer-output))
(defun buffer-output (buffer)
  "Grab the output from a buffer created with (make-buffer)."
  (declare (type fast-io::output-buffer buffer))
  (fast-io:finish-output-buffer buffer))

(declaim (inline write-to-buffer))
(defun write-to-buffer (seq buffer &optional start end)
  "Write data to a buffer created with (make-buffer)."
  (declare (type octet-vector seq)
           (type fast-io::output-buffer buffer))
  (fast-io:fast-write-sequence seq buffer (or start 0) end))

(defun do-chunk-data (data buffer write-cb &key start end new-buffer)
  "Util function that splits data into the (length buffer) chunks and calls
   write-cb for each chunk."
  (cond ((streamp data)
         (loop for read-buffer = (if new-buffer
                                     (static-vectors:make-static-vector (length buffer))
                                     buffer)
               for n = (read-sequence read-buffer data)
               while (< 0 n) do
           (funcall write-cb read-buffer n)))
        (t
          (let* ((len (length data))
                 (start (or start 0))
                 (end (min (or end len) len))
                 (data-length (- end start))
                 (data-index start)
                 (buffer-length (length buffer)))
            (loop while (< 0 data-length) do
              (let ((bufsize (min data-length buffer-length))
                    ;; create a new buffer if we ask for one.
                    ;; NOTE: the newly created buffer MUST be freed elsewhere
                    (read-buffer (if new-buffer
                                     (static-vectors:make-static-vector buffer-length)
                                     buffer)))
                (replace read-buffer data :start2 data-index :end2 end)
                (funcall write-cb read-buffer bufsize)
                (decf data-length bufsize)
                (incf data-index bufsize)))))))

(defmacro with-lock (&body body)
  "If threading is enabled, locks the current event loop before processing body
   and releases the lock after body is finished."
  `(if *enable-threading*
       (bt:with-lock-held ((event-base-lock *event-base*))
         ,@body)
       (progn ,@body)))

(defun make-pointer-eql-able (pointer)
  "Abstraction to make a CFFI pointer #'eql to itself. Does its best to be the
   most performant for the current implementation."
  (when pointer
    #+(or ccl)
      pointer
    #-(or ccl)
      (if (cffi:pointerp pointer)
          (cffi:pointer-address pointer)
          pointer)))

(defun create-data-pointer ()
  "Creates a pointer in C land that can be used to attach data/callbacks to.
   Note that this must be freed via clear-pointer-data."
  (cffi:foreign-alloc :char :count 1))

(defun save-callbacks (pointer callbacks)
  "Save a set of callbacks, keyed by the given pointer."
  (with-lock
    (let ((callbacks (if (listp callbacks)
                         callbacks
                         (list callbacks))))
      (setf (gethash (make-pointer-eql-able pointer) *function-registry*) callbacks))))

(defun get-callbacks (pointer)
  "Get all callbacks for the given pointer."
  (with-lock
    (when *function-registry*
      (gethash (make-pointer-eql-able pointer) *function-registry*))))

(defun clear-callbacks (pointer)
  "Clear out all callbacks for the given pointer."
  (with-lock
    (when *function-registry*
      (remhash (make-pointer-eql-able pointer) *function-registry*))))

(defun attach-data-to-pointer (pointer data)
  "Attach a lisp object to a foreign pointer."
  (with-lock
    (setf (gethash (make-pointer-eql-able pointer) *data-registry*) data)))

(defun deref-data-from-pointer (pointer)
  "Grab data attached to a CFFI pointer."
  (with-lock
    (when (and pointer *data-registry*)
      (gethash (make-pointer-eql-able pointer) *data-registry*))))

(defun clear-pointer-data (pointer)
  "Clear the data attached to a CFFI pointer."
  (with-lock
    (when (and pointer *data-registry*)
      (remhash (make-pointer-eql-able pointer) *data-registry*))))

(defun free-pointer-data (pointer &key preserve-pointer)
  "Clears out all data attached to a foreign pointer, and frees the pointer
   (unless :preserve-pointer is t)."
  (when pointer
    (unwind-protect
      (progn
        (clear-callbacks pointer)
        (clear-pointer-data pointer))
      (unless preserve-pointer
        (with-lock
          (when (cffi:pointerp pointer)
            (cffi:foreign-free pointer)))))))

(defun append-array (arr1 arr2)
  "Create an array, made up of arr1 followed by arr2."
  (warn "cl-async-util:append-array is worthless. please consider using make-buffer/write-to-buffer/buffer-output instead")
  (let ((arr1-length (length arr1))
        (arr2-length (length arr2)))
    (let ((arr (make-array (+ arr1-length arr2-length)
                           :element-type (array-element-type arr1))))
      (replace arr arr1 :start1 0)
      (replace arr arr2 :start1 arr1-length)
      arr)))

(defparameter *ipv4-scanner*
  (cl-ppcre:create-scanner
    "^((25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[0-9]{2}|[0-9])\\.){3}(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[0-9]{2}|[0-9])$"
    :case-insensitive-mode t)
  "Scanner that detects if a string is an IPV4 address.")

(defparameter *ipv6-scanner*
  (cl-ppcre:create-scanner
    "^\s*((([0-9A-Fa-f]{1,4}:){7}([0-9A-Fa-f]{1,4}|:))|(([0-9A-Fa-f]{1,4}:){6}(:[0-9A-Fa-f]{1,4}|((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9A-Fa-f]{1,4}:){5}(((:[0-9A-Fa-f]{1,4}){1,2})|:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9A-Fa-f]{1,4}:){4}(((:[0-9A-Fa-f]{1,4}){1,3})|((:[0-9A-Fa-f]{1,4})?:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){3}(((:[0-9A-Fa-f]{1,4}){1,4})|((:[0-9A-Fa-f]{1,4}){0,2}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){2}(((:[0-9A-Fa-f]{1,4}){1,5})|((:[0-9A-Fa-f]{1,4}){0,3}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){1}(((:[0-9A-Fa-f]{1,4}){1,6})|((:[0-9A-Fa-f]{1,4}){0,4}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(:(((:[0-9A-Fa-f]{1,4}){1,7})|((:[0-9A-Fa-f]{1,4}){0,5}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:)))(%.+)?\s*$"
    :case-insensitive-mode t)
  "Scanner that detects if a string is an IPV6 address.")

(defun ipv4-address-p (addr)
  "Determine if the given host is an IPV4 addr or a hostname."
  (cl-ppcre:scan *ipv4-scanner* addr))

(defun ipv6-address-p (addr)
  "Determine if the given host is an IPV6 addr or a hostname."
  (cl-ppcre:scan *ipv6-scanner* addr))

(defun ip-address-p (addr)
  "Determine if the given host is an IP or a hostname."
  (or (ipv4-address-p addr)
      (ipv6-address-p addr)))

(defmacro define-condition-alias (alias name)
  "Define an alias for the specified condition."
  `(progn
     (deftype ,alias () ',name)
     (setf (find-class ',alias) (find-class ',name))))
