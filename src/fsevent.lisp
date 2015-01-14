(in-package :cl-async)

(defclass fs-monitor ()
  ((c :accessor fs-monitor-c)))

(defmethod initialize-instance :after ((fs-monitor fs-monitor) &key &allow-other-keys)
  (setf (fs-monitor-c fs-monitor)
        (uv:alloc-handle :fs-event)))

(define-c-callback fs-event-callback :void ((handle :pointer) (path :string)
                                            (events :int) (status :int))
  (let* ((fs-monitor (deref-data-from-pointer handle))
         (callbacks (get-callbacks handle))
         (event-cb (getf callbacks :event-cb))
         (fs-cb (getf callbacks :fs-cb)))
    (catch-app-errors event-cb
      (if (zerop status)
          (funcall fs-cb
                   fs-monitor path
                   (plusp (logand events (cffi:foreign-enum-value 'uv:uv-fs-event :rename)))
                   (plusp (logand events (cffi:foreign-enum-value 'uv:uv-fs-event :change))))
          (run-event-cb 'event-handler status event-cb)))))

;; Fixme: copy & paste. Need common handle wrapper superclass
(define-c-callback fs-monitor-close-cb :void ((handle :pointer))
  "Called when a fs-monitor closes."
  ;; FIXME: same as streamish-close-cb
  (free-pointer-data handle :preserve-pointer t)
  (uv:free-handle handle))

(defun %fs-monitor-close (handle)
  (uv:uv-close handle (cffi:callback fs-monitor-close-cb)))

(defun fs-monitor-close (fs-monitor)
  (%fs-monitor-close (fs-monitor-c fs-monitor))
  (setf (fs-monitor-c fs-monitor) nil))

(defun fs-watch (path callback &key (event-cb #'error))
  (let* ((fs-monitor (make-instance 'fs-monitor))
         (handle (fs-monitor-c fs-monitor)))
    (let ((res (uv:uv-fs-event-init (event-base-c *event-base*) handle)))
      (unless (zerop res)
        ;; this shouldn't actually occur as no libuv backends return
        ;; non-zero result for uv_fs_event_init()
        (uv:free-handle handle)
        (event-handler res event-cb :throw t)
        (return-from fs-watch))
      (when (zerop res)
        (attach-data-to-pointer handle fs-monitor)
        (save-callbacks handle (list :fs-cb callback :event-cb event-cb))
        (setf res
              (uv:uv-fs-event-start
               handle (cffi:callback fs-event-callback)
               (namestring path)
               4 #++ 0))
        (cond ((zerop res)
               fs-monitor)
              (t
               (as:with-delay ()
                 (fs-monitor-close handle))
               (event-handler res event-cb :throw t)
               nil))))))

;; FIXME: is this really needed?
(defun fs-unwatch (fs-monitor)
  (uv:uv-fs-event-stop (fs-monitor-c fs-monitor)))

(defmethod handle-cleanup ((handle-type (eql :fs-event)) handle)
  (let ((fs-monitor (deref-data-from-pointer handle)))
    (%fs-monitor-close handle)
    (when fs-monitor
      (setf (fs-monitor-c fs-monitor) nil))))
