(in-package :cl-async-test)
(in-suite cl-async-test)

(test dns-simple
  "Test a simple DNS request"
  (is (string=
        (async-let ((lookup nil))
          (as:dns-lookup "localhost"
            (lambda (addr fam)
              (declare (ignore fam))
              (setf lookup addr))
            (lambda (ev)
              (error ev))
            :family as:+af-inet+))
        "127.0.0.1")))

(test dns-multi
  "Test multiple DNS requeests"
  (multiple-value-bind (dns1 dns2)
      (async-let ((addr1 nil)
                  (addr2 nil))
        (as:dns-lookup "google.com"
          (lambda (addr fam)
            (declare (ignore fam))
            (setf addr1 addr))
          (lambda (ev) (error ev)))
        (as:dns-lookup "musio.com"
          (lambda (addr fam)
            (declare (ignore fam))
            (setf addr2 addr))
          (lambda (ev) (error ev))))
    (is (stringp dns1))
    (is (stringp dns2))))

