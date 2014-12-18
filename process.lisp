(in-package :cl-async)

;; TBD: handle errors
;; TBD: separate tcp-stream from async-stream.
;; Also, need to rename (?) socket in tcp.lisp
;; TBD: use memset or make-foreign-type
;; TBD: custom env
;; TBD: custom process name
;; TBD: custom cwd
;; TBD: custom string encoding
;; TBD: process flags (detached etc.)

(define-c-callback process-exit-cb :void ((process :pointer) (exit-status :int64) (term-signal :int))
  (:printv "Process-exit-cb" process exit-status term-signal))

(defun init-stdio-container (container fd)
  (setf (uv-a:uv-stdio-container-t-flags container)
        (cffi:foreign-enum-value 'uv:uv-stdio-flags :+uv-inherit-fd+)
        (cffi:foreign-slot-value
         (cffi:foreign-slot-pointer container '(:struct uv:uv-stdio-container-t) 'uv::data)
         '(:union uv:uv-stdio-container-s-data)
         'uv::fd)
        fd))

(defun spawn (path args)
  (check-event-loop-running)
  (let ((handle (uv:alloc-handle :process)))
    (cffi:with-foreign-objects ((options '(:struct uv:uv-process-options-t))
                                (stdio '(:struct uv:uv-stdio-container-t) 3))
      (cffi:with-foreign-strings (#++(cwd "/tmp")
                                  (file (namestring path)))
        (cffi:with-foreign-object (c-args :pointer (+ 2 (length args)))
          (dotimes (fd 3)
            (init-stdio-container (cffi:mem-aptr stdio '(:struct uv:uv-stdio-container-t) fd)
                                  fd))
          (setf (cffi:mem-aref c-args :pointer) file
                (cffi:mem-aref c-args :pointer (1+ (length args))) (cffi:null-pointer))
          (loop for i from 1
                for arg in args
                do (setf (cffi:mem-aref c-args :pointer i)
                         (cffi:foreign-string-alloc arg)))
          (setf (uv-a:uv-process-options-t-exit-cb options) (cffi:callback process-exit-cb)
                (uv-a:uv-process-options-t-file options) file
                (uv-a:uv-process-options-t-args options) c-args
                (uv-a:uv-process-options-t-env options) (cffi:null-pointer)
                (uv-a:uv-process-options-t-cwd options) (cffi:null-pointer) #++ cwd
                (uv-a:uv-process-options-t-flags options) 0
                (uv-a:uv-process-options-t-stdio-count options) 3
                (uv-a:uv-process-options-t-stdio options) stdio
                (uv-a:uv-process-options-t-uid options) 0
                (uv-a:uv-process-options-t-gid options) 0)
          (unwind-protect
               (:printv (uv:uv-spawn (event-base-c *event-base*) handle options))
            (loop for i from 1 upto (length args)
                  do (cffi:foreign-string-free (cffi:mem-aref c-args :pointer i)))))))
    handle))
