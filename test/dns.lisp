(in-package :cl-async-test)
(in-suite cl-async-test-core)

(test dns-simple
  "Test a simple DNS request"
  (is (string=
        (async-let ((lookup nil))
          (test-timeout 3)
          (as:dns-lookup "localhost"
            (lambda (addr fam)
              (declare (ignore fam))
              (setf lookup addr))
            :event-cb (lambda (ev) (error ev))
            :family as:+af-inet+))
        "127.0.0.1")))

(test dns-multi
  "Test multiple DNS requests"
  (multiple-value-bind (dns1 dns2)
      (async-let ((addr1 nil)
                  (addr2 nil))
        (test-timeout 5)
        (as:dns-lookup "google.com"
          (lambda (addr fam)
            (declare (ignore fam))
            (setf addr1 addr))
          :event-cb (lambda (ev) (error ev)))
        (as:dns-lookup "localhost"
          (lambda (addr fam)
            (declare (ignore fam))
            (setf addr2 addr))
          :event-cb (lambda (ev) (error ev))))
    (is (stringp dns1))
    (is (stringp dns2))))

(test dns-lookup-ipv4
  "Test IPV4 family"
  (multiple-value-bind (ipv4)
      (async-let ((ipv4 nil))
        (test-timeout 3)
        (as:dns-lookup "google.com"
          (lambda (addr fam)
            (declare (ignore fam))
            (setf ipv4 addr))
          :event-cb (lambda (ev) (error ev))
          :family as:+af-inet+))
    (is (cl-async-util::ipv4-address-p ipv4))))

(test dns-lookup-ipv6
  "Test IPV6 family (can fail in *nix)"
  (multiple-value-bind (ipv6)
      (handler-case
        (async-let ((ipv6 nil))
          (test-timeout 3)
          (as:dns-lookup "google.com"
            (lambda (addr fam)
              (declare (ignore fam))
              (setf ipv6 addr))
            :event-cb (lambda (ev) (error ev))
            :family as:+af-inet6+))
        (error (e) (format nil "(~a) ~a" (as:event-errcode e) (as:event-errmsg e))))
    (is (cl-async-util::ipv6-address-p ipv6))))

(test dns-fail
  "Tests DNS failure on fake host, makes sure event-cb gets fires once"
  (let ((num-err 0))
    (signals as:dns-error
      (async-let ((lookup nil))
        (test-timeout 3)
        (as:dns-lookup "all your children are poor unfortunate victims of lies you believe."
          (lambda (addr fam) (list addr fam))
          :event-cb (lambda (ev)
                      (incf num-err)
                      (error ev)))))
    (is (= num-err 1))))

(test reverse-dns-lookup-ipv4
  "Test IPV4 family"
  (multiple-value-bind (host)
      (async-let ((host nil))
        (test-timeout 3)
        (as:reverse-dns-lookup "8.8.8.8"
          (lambda (host* service)
            (declare (ignore service))
            (setf host host*))
          :event-cb (lambda (ev) (error ev))))
    (is (string= host "google-public-dns-a.google.com"))))

(test reverse-dns-lookup-ipv6
  "Test IPV6 family"
  (multiple-value-bind (host)
      (handler-case
        (async-let ((host nil))
          (test-timeout 3)
          (as:reverse-dns-lookup "2001:4860:4860::8888"
            (lambda (host* service)
              (declare (ignore service))
              (setf host host*))
            :event-cb (lambda (ev) (error ev))))
        (error (e) (format nil "(~a) ~a" (as:event-errcode e) (as:event-errmsg e))))
    (is (string= host "google-public-dns-a.google.com"))))

(test dns-lookup-mem-leak
  "Test dns-lookup memory leaks"
  (is (async-let ((counts-equal nil))
        (test-timeout 3)
        (let ((old-count (hash-table-count cl-async-base:*function-registry*)))
          (as:dns-lookup "localhost"
            (lambda (addr fam)
              (declare (ignore addr fam))
              (as:delay
                (lambda ()
                  (setf counts-equal
                        (= old-count
                           (hash-table-count cl-async-base:*function-registry*))))))
            :event-cb (lambda (ev) (error ev))
            :family as:+af-inet+)))))

(test reverse-dns-lookup-mem-leak
  "Test reverse-dns-lookup memory leaks"
  (is (async-let ((counts-equal nil))
        (test-timeout 3)
        (let ((old-count (hash-table-count cl-async-base:*function-registry*)))
          (as:reverse-dns-lookup "8.8.8.8"
            (lambda (host service)
              (declare (ignore host service))
              (as:delay
                (lambda ()
                  (setf counts-equal
                        (= old-count
                           (hash-table-count cl-async-base:*function-registry*))))))
            :event-cb (lambda (ev) (error ev)))))))
