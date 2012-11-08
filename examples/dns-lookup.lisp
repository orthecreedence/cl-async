;;; Some examples of async DNS lookups.

(ql:quickload :cl-async)

(defun do-lookups ()
  (dolist (lookup `(("google.com")
                    ("musio.com" . ,as:+af-inet+)
                    ("www.google.com" . ,as:+af-inet6+)))
    (let ((host (car lookup))
          (family (if (cdr lookup) (cdr lookup) as:+af-unspec+)))
      (as:dns-lookup host
        (lambda (addr fam)
          (declare (ignore fam))
          (format t "~a resolved to ~s (~s)~%" host addr family))
        (lambda (ev)
          (format t "ev: ~a(~a): ~a~%" host family ev))
        :family family))))

(as:start-event-loop #'do-lookups)
