(in-package :cl-async)

;; TBD: handle errors
;; TBD: proper exit callback
;; TBD: support :STREAM besides :PIPE (to get async stream)
;; TBD: separate tcp-stream from async-stream.
;; Also, need to rename (?) socket in tcp.lisp
;; TBD: destroy process handles on exit
;; TBD: utf-8 pipe support
;; TBD: check process handles during walk
;; TBD: custom env
;; TBD: custom process name
;; TBD: custom cwd
;; TBD: custom string encoding
;; TBD: process flags (detached etc.)
;; TBD: process-kill
;; TBD: close streams on exit

;; TBD: use common superclass for libuv handle wrappers

(defclass process ()
  ((c :accessor process-c :initarg :c)
   (input :accessor process-input :initarg :input)
   (output :accessor process-output :initarg :output)
   (error-output :accessor process-error-output :initarg :error-output)))

(define-c-callback process-exit-cb :void ((process :pointer) (exit-status :int64) (term-signal :int))
  (:printv "Process-exit-cb" process exit-status term-signal))

(defun init-stdio-container (container out-p type fd pipe-args)
  (setf (uv-a:uv-stdio-container-t-flags container)
        (flet ((v (name)
                 (cffi:foreign-enum-value 'uv:uv-stdio-flags name)))
          (ecase type
            (:ignore (v :+uv-ignore+))
            (:inherit (v :+uv-inherit-fd+))
            (:pipe
             (:printv
              (logior (v :+uv-create-pipe+)
                      (if out-p
                          (v :+uv-writable-pipe+)
                          (v :+uv-readable-pipe+))))))))
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
             (:printv (streamish-c pipe)))
       pipe))))

(defun spawn (path args &key (input :ignore) (output :ignore) (error-output :ignore))
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
            (unwind-protect
                 (:printv (uv:uv-spawn (event-base-c *event-base*) handle options))
              (loop for i from 1 upto (length args)
                    do (cffi:foreign-string-free (cffi:mem-aref c-args :pointer i))))
            (loop for pipe in pipes
                  for in-p = t then nil
                  when (and pipe (or in-p (streamish-read-start pipe)))
                    do (setf (socket-connected pipe) t)
                       (write-pending-socket-data pipe))
            (apply #'values
                   (make-instance 'process
                                  :c handle
                                  :input (first pipes)
                                  )
                   pipes)))))))

(defun tst ()
  (multiple-value-bind (process stdin)
      (spawn "bash" '("-c" "cat 1>&2")
             :input (list :pipe :data "hello")
             :error-output (list :pipe
                                 :read-cb #'(lambda (p r)
                                              (:printv r)
                                              (:printv p (babel:octets-to-string r)))))
    (declare (ignore process))
    (streamish-write stdin (babel:string-to-octets (format nil "abc~%")))
    (close-streamish stdin)))
