(in-package :cl-async)

(defgeneric streamish (thing)
  (:documentation "Returned associated streamish for THING or THING itself
  if THING is a streamish."))

(defgeneric errno-event (streamish errno)
  (:documentation "Make an event based on errno and streamish."))

(defmethod errno-event ((streamish t) (errno t))
  (make-instance 'event-error :code errno :msg (error-str errno)))

(defmethod errno-event ((streamish t) (errno (eql (uv:errval :eai-noname))))
  (make-instance 'dns-error :code errno :msg "DNS lookup fail"))

(defmethod errno-event ((streamish t) (errno (eql (uv:errval :efault))))
  (make-instance 'event-error :code errno :msg "bad address in system call argument"))

(defun event-handler (error event-cb &key streamish throw)
  "Called when an event (error, mainly) occurs."
  ;; here we check if errno is actually an event/error object passed in
  ;; directly. if so, we kindly forward it along to the event-cb.
  (let* ((errno (when (numberp error) error))
         (event (if (numberp error)
                    (errno-event streamish errno)
                    error)))
    (macrolet ((closing-streamish-afterwards (&body body)
                 `(unwind-protect
                      (if (subtypep (type-of event) 'streamish-error)
                          (as:delay (lambda () ,@body))
                          (progn ,@body))
                    ;; if the app closed the streamish in the event cb (perfectly fine),
                    ;; make sure we don't trigger an error trying to close it again.
                    (when (and streamish (not (streamish-closed-p streamish)))
                      (close-streamish streamish :force t)))))
      (if throw
          (error event)
          (closing-streamish-afterwards
            (when event-cb
              (funcall event-cb event)))))))

;; TBD: seems like streamish-info/socket-info/tcp-info/event-info isn't actually used?
(define-condition streamish-info (event-info)
  ;; initarg :socket is added compatibility
  ((streamish :initarg :streamish
              :initarg :socket
              :accessor streamish
              :accessor tcp-socket ;; compatibility
              :initform nil))
  (:report (lambda (c s)
             (print-unreadable-object (c s :type t :identity t)
               (format s "~a" (streamish c)))))
  (:documentation "Base streamish condition. Holds the streamish object."))

(define-condition streamish-error (event-error streamish-info) ()
  (:report (lambda (c s)
             (print-unreadable-object (c s :type t :identity t)
               (format s "~a: ~a: ~a" (streamish c) (event-errcode c) (event-errmsg c)))))
  (:documentation "Describes a general streamish error."))

(define-condition streamish-enoent (streamish-error) ()
  (:documentation "Passed to an event callback on Error: no such file or directory."))

(define-condition streamish-eof (streamish-info) ()
  (:documentation "Passed to an event callback when stream EOF is reached."))

(define-condition streamish-closed (streamish-error) ()
  (:report (lambda (c s) (format s "Closed streamish being operated on: ~a." (streamish c))))
  (:documentation "Thrown when a closed streamish is being operated on."))

(define-condition streamish-broken-pipe (streamish-error) ()
  (:report (lambda (c s) (format s "Broken pipe: ~a" (streamish c))))
  (:documentation "Broken pipe."))

(define-condition streamish-canceled (streamish-error) ()
  (:report (lambda (c s) (format s "Operation canceled: ~a" (streamish c))))
  (:documentation "Operation canceled."))

(defclass streamish ()
  ((c :accessor streamish-c :initarg :c :initform (cffi:null-pointer))
   (data :accessor streamish-data :initarg data :initform nil
     :documentation "Used to store arbitrary (app-defined) data with a streamish.")
   (closed :accessor streamish-closed :initarg :closed :initform nil)
   (drain-read-buffer :accessor streamish-drain-read-buffer :initarg :drain-read-buffer :initform t))
  (:documentation "Wraps around a streamish."))

(defmethod streamish ((streamish streamish))
  streamish)

(defmethod errno-event ((streamish streamish) (errno t))
  (make-instance 'streamish-error
                 :streamish streamish
                 :code errno
                 :msg (error-str errno)))

(defmethod errno-event ((streamish streamish) (errno (eql (uv:errval :enoent))))
  (make-instance 'streamish-enoent :streamish streamish))

(defmethod errno-event ((streamish streamish) (errno (eql (uv:errval :eof))))
  (make-instance 'streamish-eof :streamish streamish))

(defmethod errno-event ((streamish streamish) (errno (eql (uv:errval :epipe))))
  (make-instance 'streamish-broken-pipe :streamish streamish))

(defmethod errno-event ((streamish streamish) (errno (eql (uv:errval :ecanceled))))
  (make-instance 'streamish-canceled :streamish streamish))

(defun check-streamish-open (streamish)
  "Throw a streamish-closed condition if given a streamish that's closed."
  (when (and (typep streamish 'streamish)
             (or (streamish-closed streamish)))
    (error 'streamish-closed :code -1 :msg "Trying to operate on a closed streamish" :streamish streamish)))

(defun streamish-closed-p (streamish)
  "Return whether a streamish is closed or not."
  (streamish-closed streamish))

(defgeneric close-streamish (streamish &key &allow-other-keys)
  (:documentation
    "Free a streamish (uvstream) and clear out all associated data."))

(defun do-close-streamish (uvstream &key force)
  "Close an UV stream."
  (unless (zerop (uv:uv-is-closing uvstream))
    (return-from do-close-streamish))
  (uv:uv-read-stop uvstream)
  (cond ((or force (zerop (uv:uv-is-writable uvstream)))
         (uv:uv-close uvstream (cffi:callback streamish-close-cb)))
        (t
         (let* ((shutdown-req (uv:alloc-req :shutdown))
                (r (uv:uv-shutdown shutdown-req uvstream (cffi:callback streamish-shutdown-cb))))
           (if (zerop r)
               (attach-data-to-pointer shutdown-req (list uvstream))
               (uv:uv-close uvstream (cffi:callback streamish-close-cb)))))))

(defmethod close-streamish ((streamish streamish) &key force)
  "Close and free a streamish and all of it's underlying structures."
  (when (streamish-closed-p streamish)
    (return-from close-streamish))
  (check-streamish-open streamish)
  (let* ((uvstream (streamish-c streamish))
         (data (deref-data-from-pointer uvstream))
         (read-timeout (car (getf data :read-timeout)))
         (write-timeout (car (getf data :write-timeout))))
    (dolist (timeout (list read-timeout
                           write-timeout))
      (when (and timeout
                 (not (event-freed-p timeout)))
        (free-event timeout)))
    (setf (streamish-closed streamish) t)
    (do-close-streamish uvstream
                       :force force)))

(defun write-to-uvstream (uvstream data &key start end)
  "Util function to write data directly to a uv stream object."
  (let* ((start (or start 0))
         (end (or end (length data)))
         (bufsize (- end start))
         (buffer (static-vectors:make-static-vector bufsize)))
    (replace buffer data :start2 start :end2 end)
    (let ((req (uv:alloc-req :write))
          (buf (uv:alloc-uv-buf (static-vectors:static-vector-pointer buffer) bufsize)))
      (let ((res (uv:uv-write req uvstream buf 1 (cffi:callback streamish-write-cb))))
        (uv:free-uv-buf buf)
        (unless (zerop res)
          (let ((streamish (getf (deref-data-from-pointer uvstream) :streamish)))
            (uv:free-req req)
            (error (errno-event streamish res))))
        (attach-data-to-pointer req (list :uvstream uvstream :buffer buffer))))))

(define-c-callback streamish-shutdown-cb :void ((req :pointer) (status :int))
  "Called when a streamish shuts down."
  (declare (ignore status))
  (uv:free-req req)
  (let ((uvstream (car (deref-data-from-pointer req))))
    (free-pointer-data req :preserve-pointer t)
    (when (zerop (uv:uv-is-closing uvstream))
      (uv:uv-close uvstream (cffi:callback streamish-close-cb)))))

(defgeneric streamish-write (streamish data &key start end &allow-other-keys)
  (:documentation
    "Write data into a streamish. Allows specifying read/write/event
     callbacks. Any callback left nil will use that current callback from the
     streamish (so they only override when specified, otherwise keep the current
     callback).

     Note that libuv doesn't buffer output for non-connected sockets, so we have
     to do it ourselves by checking if the socket is connected and buffering
     accordingly."))

(defun streamish-convert-data (data)
  (if (stringp data)
      (babel:string-to-octets data :encoding :utf-8)
      data))

(defmethod streamish-write ((streamish streamish) data &key start end &allow-other-keys)
  (unless (streamish-closed-p streamish)
    (write-to-uvstream (streamish-c streamish)
                       (streamish-convert-data data)
                       :start start :end end)))

(defmethod streamish-write :around ((streamish streamish) data
                                    &key read-cb write-cb event-cb
                                    &allow-other-keys)
  (if (or read-cb write-cb event-cb)
      ;; we're specifying callbacks. since we're most likely calling this from
      ;; inside a streamish callback and we don't necessarily want to overwrite
      ;; that streamish' callbacks until it finishes, we set a delay here so the
      ;; callback binding happens after the caller returns to the event loop.
      (as:delay
       (lambda ()
         (let ((callbacks (get-callbacks (streamish-c streamish))))
           (save-callbacks (streamish-c streamish)
                           (list :read-cb (or read-cb (getf callbacks :read-cb))
                                 :write-cb (or write-cb (getf callbacks :write-cb))
                                 :event-cb (or event-cb (getf callbacks :event-cb))))
           (call-next-method))))

      ;; we're not setting callbacks, so just enable the streamish and send the
      ;; data
      (call-next-method)))

(define-c-callback streamish-alloc-cb :void ((handle :pointer) (size :unsigned-int) (buf :pointer))
  "Called when we want to allocate data to be filled for stream reading."
  (declare (ignore handle))
  (uv:alloc-uv-buf (static-vectors:static-vector-pointer *input-buffer*) (min size *buffer-size*) buf))

(define-c-callback streamish-read-cb :void ((uvstream :pointer) (nread :int) (buf :pointer))
  "Called when a stream has been read into a buffer returned by alloc-cb."
  (declare (ignore buf))
  (let* ((stream-data (deref-data-from-pointer uvstream))
         (read-timeout (getf stream-data :read-timeout))
         (timeout (car read-timeout))
         (streamish (getf stream-data :streamish))
         (stream (getf stream-data :stream))
         (drain-read (streamish-drain-read-buffer streamish))
         (callbacks (get-callbacks uvstream))
         (read-cb (getf callbacks :read-cb))
         (event-cb (getf callbacks :event-cb)))
    (catch-app-errors event-cb
      (when (< nread 0)
        ;; we got an error
        (run-event-cb 'event-handler nread event-cb :streamish streamish)
        (return-from streamish-read-cb))

      ;; reset the read timeout
      (when timeout
        (remove-event timeout)
        (add-event timeout :timeout (cdr read-timeout)))

      ;; read the buffer
      (let ((bytes (make-array nread :element-type 'octet)))
        ;; input buffer was given to libuv in the alloc-cb, so we can just pull
        ;; data directly out of it now
        (replace bytes *input-buffer*)
        (cond ((and read-cb drain-read)
               ;; we're draining here, so call our read callback
               (funcall read-cb streamish bytes))
              (read-cb
               ;; we're not draining and we have a read CB, so stream
               (stream-append-bytes stream bytes)
               (when read-cb (funcall read-cb streamish stream))))))))

(define-c-callback streamish-write-cb :void ((req :pointer) (status :int))
  "Called when data is finished being written to a streamish."
  (let* ((data (deref-data-from-pointer req))
         (uvstream (getf data :uvstream))
         (buffer (getf data :buffer))
         (streamish (getf (deref-data-from-pointer uvstream) :streamish))
         (callbacks (get-callbacks uvstream))
         (write-cb (getf callbacks :write-cb))
         (event-cb (getf callbacks :event-cb)))
    (catch-app-errors event-cb
      (static-vectors:free-static-vector buffer)
      (free-pointer-data req :preserve-pointer t)
      (uv:free-req req)
      (if (zerop status)
          (when write-cb
            (funcall write-cb streamish))
          (run-event-cb 'event-handler status event-cb :streamish streamish)))))

(define-c-callback streamish-close-cb :void ((uvstream :pointer))
  "Called when a streamish closes."
  ;; !!NOTE!! This callback is used for both tcp clients AND servers! if either
  ;; ever needs special treatment, split out the callbacks
  (free-pointer-data uvstream :preserve-pointer t)
  (uv:free-handle uvstream))

(defun streamish-read-start (streamish)
  "Start reading on the socket, return true on success.
  Invoke streamish' event handler callback on error,
  returning NIL."
  (let* ((uvstream (streamish-c streamish))
         (event-cb (getf (get-callbacks uvstream) :event-cb))
         (res (uv:uv-read-start uvstream
                                (cffi:callback streamish-alloc-cb)
                                (cffi:callback streamish-read-cb))))
    (if (zerop res)
        t
        (run-event-cb 'event-handler res event-cb :streamish streamish))))
