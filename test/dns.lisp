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

(test dns-lookup-ipv4
  "Test IPV4 family"
  (is (as::ipv4-address-p
        (async-let ((ipv4 nil))
          (as:dns-lookup "google.com"
            (lambda (addr fam)
              (declare (ignore fam))
              (setf ipv4 addr))
            (lambda (ev)
              (error ev))
            :family as:+af-inet+)))))

(test dns-lookup-ipv6
  "Test IPV6 family: can fail in linux, for some reason (Slack, at least)"
  (is (as::ipv6-address-p
        (handler-case
          (async-let ((ipv6 nil))
            (as:dns-lookup "google.com"
              (lambda (addr fam)
                (declare (ignore fam))
                (setf ipv6 addr))
              (lambda (ev)
                (error ev))
              :family as:+af-inet6+))
          (t (e) (as:conn-errmsg e))))))

(test dns-fail
  "Tests DNS failure on fake host, makes sure event-cb gets fires once"
  (let ((num-err 0))
    (signals as:dns-error
      (async-let ((lookup nil))
        (as:dns-lookup "all your children are poor unfortunate victims of lies you believe."
          (lambda (addr fam) (list addr fam))
          (lambda (ev)
            (incf num-err)
            (error ev)))))
    (is (= num-err 1))))

