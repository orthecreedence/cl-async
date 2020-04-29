(in-package :cl-async-test)
(in-suite cl-async-test-core)

(test spawn-simple
  (with-test-event-loop ()
    (test-timeout 3)
    (with-path-under-tmpdir (path "blabla")
      (let (process)
        (setf process
              (as:spawn "touch" (list (namestring path))
                        :exit-cb (called-once
                                  #'(lambda (act-process exit-status term-signal)
                                      (is-true (uiop:file-exists-p path))
                                      (is (eq process act-process))
                                      (is (= 0 exit-status))
                                      (is (= 0 term-signal))))))))))

(test spawn-redirect-output
  (with-test-event-loop ()
    (test-timeout 3)
    (let ((bytes (make-array 0 :element-type 'octet))
          process input output error-output)
      (multiple-value-setq (process input output error-output)
        (as:spawn "cat" '()
                  :input (list :pipe :data "hello")
                  :exit-cb (called-once
                            #'(lambda (proc exit-status term-signal)
                                (is (eq process proc))
                                (is (= 0 exit-status))
                                (is (= 0 term-signal))))
                  :output (list :pipe
                                :read-cb #'(lambda (pipe data)
                                             (is (eq output pipe))
                                             (setf bytes (concatenate '(vector octet) data))))))
      (is (null error-output))
      (is (not (null output)))
      (is (eq input (as:process-input process)))
      (is (eq output (as:process-output process)))
      (is (eq error-output (as:process-error-output process)))
      (as:write-socket-data input (format nil "--42--"))
      (wait (string= "hello--42--" (babel:octets-to-string bytes)))
      (as:close-socket input))))

(test spawn-redirect-error-output
  (with-test-event-loop ()
    (test-timeout 3)
    (let ((bytes (make-array 0 :element-type 'octet))
          process input output error-output)
      (multiple-value-setq (process input output error-output)
          (as:spawn "bash" '("-c" "cat 1>&2")
                    :input (list :pipe :data "hello")
                    :exit-cb (called-once
                              #'(lambda (proc exit-status term-signal)
                                  (is (eq process proc))
                                  (is (= 0 exit-status))
                                  (is (= 0 term-signal))))
                    :error-output (list :pipe
                                        :read-cb #'(lambda (pipe data)
                                                     (is (eq error-output pipe))
                                                     (setf bytes (concatenate '(vector octet) data))))))
      (is (not (null error-output)))
      (is (null output))
      (is (eq input (as:process-input process)))
      (is (eq output (as:process-output process)))
      (is (eq error-output (as:process-error-output process)))
      (as:write-socket-data input (format nil "--42--"))
      (wait (string= "hello--42--" (babel:octets-to-string bytes)))
      (as:close-socket input))))

(test spawn-redirect-stream
  (with-test-event-loop ()
    (test-timeout 3)
    (let ((bytes (make-array 0 :element-type 'octet))
          process input output error-output)
      (multiple-value-setq (process input output error-output)
        (as:spawn "cat" '()
                  :input (list :stream :data "hello")
                  :exit-cb (called-once
                            #'(lambda (proc exit-status term-signal)
                                (is (eq process proc))
                                (is (= 0 exit-status))
                                (is (= 0 term-signal))))
                  :output (list :stream
                                :read-cb #'(lambda (pipe stream)
                                             (is (eq (as:streamish output) pipe))
                                             (let ((buf (make-array 128 :element-type 'octet)))
                                               (loop for n = (read-sequence buf stream)
                                                     while (plusp n) do
                                                       (setf bytes (concatenate
                                                                    '(vector octet)
                                                                    bytes
                                                                    (subseq buf 0 n)))))))))
      (is (null error-output))
      (is (not (null output)))
      (is (eq input (as:process-input process)))
      (is (eq output (as:process-output process)))
      (is (eq error-output (as:process-error-output process)))
      (write-sequence (babel:string-to-octets (format nil "--42--")) input)
      (wait (string= "hello--42--" (babel:octets-to-string bytes)))
      (close input))))

(test spawn-exec-failure ()
  (signals as:filesystem-enoent
    (with-test-event-loop ()
      (with-path-under-tmpdir (path "blabla")
        (as:spawn path '() :exit-cb #'never)
        (as:with-delay (1) nil)))))

(test process-kill ()
  (with-test-event-loop ()
    (let (process)
      (test-timeout 3)
      (setf process (as:spawn "cat" '()
                              :exit-cb (called-once
                                        #'(lambda (proc exit-status term-signal)
                                            (is (eq process proc))
                                            (is (= 0 exit-status))
                                            (is (= 2 term-signal))))
                              :input (list :pipe)))
      ;; send SIGINT
      (as:process-kill process 2))))

(test process-env
  (with-test-event-loop ()
    (with-temporary-directory (dir)
      (test-timeout 3)
      (let ((env "$HOME/.shinit")
            (dir-without-final-slash
              (subseq (namestring dir) 0 (1- (length (namestring dir)))))
            (bytes (make-array 0 :element-type 'octet)))
        (as:spawn "bash" '("-c" "echo -n $ENV $PWD")
                  :output (list :pipe
                                :read-cb #'(lambda (pipe data)
                                             (declare (ignore pipe))
                                             (setf bytes (concatenate '(vector octet) data))))
                  :env `(("ENV" . ,env))
                  :working-directory dir)

        ;; the folder on macOS has a '/private' prefixed when it got the data from the process.
        ;; i.e:
        ;; /private/var/folders/2z/831jl36d01317d4pz3g2_j100000gn/T/as-tst-rXjQJL
        ;; /var/folders/2z/831jl36d01317d4pz3g2_j100000gn/T/as-tst-rXjQJL
        (wait (let* ((actual-string (babel:octets-to-string bytes))
                     (expected-string (format nil "$HOME/.shinit ~A" dir-without-final-slash))
                     (mac-expected-string (format nil "$HOME/.shinit /private~A" dir-without-final-slash)))
                (or (string= expected-string actual-string)
                    (string= mac-expected-string actual-string))))))))
