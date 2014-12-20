(in-package :cl-async)

;; TBD: handle errors
;; TBD: proper exit callback
;; TBD: support :STREAM besides :PIPE (to get async stream)
;; TBD: separate tcp-stream from async-stream.
;; Also, need to rename (?) socket in tcp.lisp
;; TBD: destroy process handles on exit
;; TBD: utf-8 pipe support
;; TBD: check process handles during walk using generic function
;; TBD: custom env
;; TBD: custom process name
;; TBD: custom cwd
;; TBD: custom string encoding
;; TBD: process flags (detached etc.)
;; TBD: process-kill
;; TBD: close streams on exit
;; TBD: make sure process handles are freed automagically when the event loop is finished

;; TBD: use common superclass for libuv handle wrappers

(defclass process ()
  ((c :accessor process-c :initarg :c)
   (input :accessor process-input :initarg :input)
   (output :accessor process-output :initarg :output)
   (error-output :accessor process-error-output :initarg :error-output)))

(defmethod initialize-instance :after ((process process) &key exit-cb event-cb &allow-other-keys)
  (attach-data-to-pointer (process-c process) process)
  (save-callbacks (process-c process)
                  (list :exit-cb exit-cb :event-cb event-cb)))

(define-c-callback process-close-cb :void ((process-handle :pointer))
  "Called when a process closes."
  ;; FIXME: same as streamish-close-cb
  (free-pointer-data process-handle :preserve-pointer t)
  (uv:free-handle process-handle))

(defun process-close (process-handle)
  (uv:uv-close process-handle (cffi:callback process-close-cb)))

(define-c-callback process-exit-cb :void ((process-handle :pointer)
                                          (exit-status :int64)
                                          (term-signal :int))
  (let* ((process (deref-data-from-pointer process-handle))
         (callbacks (get-callbacks process-handle))
         (event-cb (getf callbacks :event-cb))
         (exit-cb (getf callbacks :exit-cb)))
    (catch-app-errors event-cb
      (when exit-cb
        (funcall exit-cb process exit-status term-signal))
      (process-close process-handle)
      (setf (process-c process) nil))))

(defun init-stdio-container (container out-p type fd pipe-args)
  (setf (uv-a:uv-stdio-container-t-flags container)
        (flet ((v (name)
                 (cffi:foreign-enum-value 'uv:uv-stdio-flags name)))
          (ecase type
            (:ignore (v :+uv-ignore+))
            (:inherit (v :+uv-inherit-fd+))
            (:pipe
             (logior (v :+uv-create-pipe+)
                     (if out-p
                         (v :+uv-writable-pipe+)
                         (v :+uv-readable-pipe+)))))))
  (case type
    (:inherit
     (setf (cffi:foreign-slot-value
            (cffi:foreign-slot-pointer container '(:struct uv:uv-stdio-container-t) 'uv::data)
            '(:union uv:uv-stdio-container-s-data)
            'uv::fd)
           fd)
     nil)
    (:pipe
     (let ((pipe (apply #'init-client-socket 'pipe
                        :allow-other-keys t pipe-args)))
       (setf (uv-a:uv-stdio-container-t-data container)
             (streamish-c pipe))
       pipe))))

(defun spawn (path args &key exit-cb
                          (event-cb #'error)
                          (input :ignore)
                          (output :ignore)
                          (error-output :ignore))
  (check-event-loop-running)
  (let ((handle (uv:alloc-handle :process)))
    (cffi:with-foreign-objects ((stdio '(:struct uv:uv-stdio-container-t) 3)
                                (c-args :pointer (+ 2 (length args))))
      (cffi:with-foreign-strings (#++ (cwd "/tmp")
                                  (file (namestring path)))
        (let ((pipes
                (loop for fd from 0 below 3
                      for (type . other-args) in (mapcar #'alexandria:ensure-list
                                                         (list input output error-output))
                      for out-p = nil then t
                      collect
                      (init-stdio-container
                       (cffi:mem-aptr stdio '(:struct uv:uv-stdio-container-t) fd)
                       out-p type fd other-args))))
          (setf (cffi:mem-aref c-args :pointer) file
                (cffi:mem-aref c-args :pointer (1+ (length args))) (cffi:null-pointer))
          (loop for i from 1
                for arg in args
                do (setf (cffi:mem-aref c-args :pointer i)
                         (cffi:foreign-string-alloc arg)))
          (with-foreign-object* (options uv:uv-process-options-t)
                                ((uv-a:uv-process-options-t-exit-cb (cffi:callback process-exit-cb))
                                 (uv-a:uv-process-options-t-file file)
                                 (uv-a:uv-process-options-t-args c-args)
                                 #++
                                 (uv-a:uv-process-options-t-env (cffi:null-pointer))
                                 #++
                                 (uv-a:uv-process-options-t-cwd (cffi:null-pointer))
                                 #++
                                 (uv-a:uv-process-options-t-flags 0)
                                 (uv-a:uv-process-options-t-stdio-count 3)
                                 (uv-a:uv-process-options-t-stdio stdio)
                                 #++
                                 (uv-a:uv-process-options-t-uid 0)
                                 #++
                                 (uv-a:uv-process-options-t-gid 0))
            (let ((res (uv:uv-spawn (event-base-c *event-base*) handle options)))
              (cond ((zerop res)
                     (loop for i from 1 upto (length args)
                           do (cffi:foreign-string-free (cffi:mem-aref c-args :pointer i)))
                     (loop for pipe in pipes
                           for in-p = t then nil
                           when (and pipe (or in-p (streamish-read-start pipe)))
                             do (setf (socket-connected pipe) t)
                                (write-pending-socket-data pipe))
                     (apply #'values
                            (make-instance 'process
                                           :c handle
                                           :exit-cb exit-cb
                                           :event-cb event-cb
                                           :input (first pipes))
                            pipes))
                    (t
                     (process-close handle)
                     (event-handler res event-cb :catch-errors t))))))))))

(defmethod handle-cleanup ((handle-type (eql :process)) handle)
  (let ((process (deref-data-from-pointer handle)))
    (process-close handle)
    (when process
      (setf (process-c process) nil))))
