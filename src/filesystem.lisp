(in-package :cl-async)

;; TBD: path encoding

(define-condition filesystem-error (event-error) ()
  (:documentation "Base class for filesystem conditions"))

(define-condition filesystem-enoent (filesystem-error) ()
  (:documentation "Error: no such file or directory"))

(define-condition filesystem-eacces (filesystem-error) ()
  (:documentation "Error: access denied"))

(define-condition filesystem-eperm (filesystem-error) ()
  (:documentation "Error: permission denied"))

(defmethod errno-event ((streamish t) (errno (eql (uv:errval :enoent))))
  (make-instance 'filesystem-enoent :code errno :msg (error-str errno)))

(defmethod errno-event ((streamish t) (errno (eql (uv:errval :eacces))))
  (make-instance 'filesystem-eacces :code errno :msg (error-str errno)))

(defmethod errno-event ((streamish t) (errno (eql (uv:errval :eperm))))
  (make-instance 'filesystem-eperm :code errno :msg (error-str errno)))

(define-c-callback fs-cb :void ((req :pointer))
  (let* ((callbacks (get-callbacks req))
         (cb (getf callbacks :fs-cb))
         (event-cb (getf callbacks :event-cb)))
    (catch-app-errors event-cb
      (unwind-protect
          (let ((res (uv-a:uv-fs-s-result req)))
            (if (zerop res)
                (funcall cb (uiop:ensure-directory-pathname
                              (uv-a:uv-fs-s-path req)))
                (run-event-cb 'event-handler res event-cb)))
        (uv:uv-fs-req-cleanup req)
        (free-pointer-data req :preserve-pointer t)
        (uv:free-req req)))))

(defun mkdtemp (template cb &key (event-cb #'error))
  (check-event-loop-running)
  (let ((tpl (namestring template)))
    (let* ((req (uv:alloc-req :fs))
           (res (uv:uv-fs-mkdtemp (event-base-c *event-base*)
                                  req tpl (cffi:callback fs-cb))))
      (cond ((zerop res)
             (save-callbacks req (list :fs-cb cb
                                       :event-cb event-cb)))
            (t
             (uv:uv-fs-req-cleanup req)
             (uv:free-req req)
             (event-handler res event-cb :throw t))))))

