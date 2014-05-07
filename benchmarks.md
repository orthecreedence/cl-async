---
title: Benchmarks
layout: default
---

Benchmarks
==========
So far, benchmarks are favorable. From my intial profiling, it seems most of the
time is spent in CFFI when on Windows, but in linux (of course) CFFI is a minor
speed bump, and the actual `cl-async:*` functions are the main slowdown (which is
good). Because of this, I really recommend running any production server on
linux. This isn't so much because Windows sucks, but because I feel like most
lisp implementations focus on linux performance a lot more than Windows (at
least when it comes to CFFI).

On my (already crowded) Linode 512, cl-async (for both
[tcp-server](/cl-async/tcp#tcp-server) was able to process about 40K
concurrent requests with this example before running out of memory:

{% highlight cl %}
(defparameter *http-response*
  (babel:string-to-octets
    (with-output-to-string (s)
      (format s "HTTP/1.1 200 OK~c~c" #\return #\newline)
      (format s "Date: Wed, 03 Oct 2012 23:43:10 GMT~c~c" #\return #\newline)
      (format s "Content-Type: text/plain~c~c" #\return #\newline)
      (format s "Content-Length: 9~c~c" #\return #\newline)
      (format s "~c~c" #\return #\newline)
      (format s "omglolwtf"))))

(defun tcp-server-test (&key stats)
  (as:start-event-loop
    (lambda ()
      (format t "Starting TCP server.~%")
      (let ((listener nil)
            (quit nil)
            (finished-requests 0)
            (last-finished 0)
            (last-time 0))
        (setf listener
              (as:tcp-server nil 9009
                             (lambda (socket data)
                               (declare (ignore data))
                               (as:delay (lambda ()
                                           (unless (as:socket-closed-p socket)
                                             (as:write-socket-data
                                               socket *http-response*
                                               :write-cb (lambda (socket)
                                                           (as:close-socket socket)
                                                           (incf finished-requests)))))
                                         :time 5))
                             (lambda (err)
                               (format t "tcp server event: ~a~%" err))))
        (as:signal-handler 2 (lambda (sig)
                               (declare (ignore sig))
                               (setf quit t)
                               (as:free-signal-handler 2)
                               (as:close-tcp-server listener)))
        (labels ((show-stats ()
                   (let* ((stats (as:stats))
                          (incoming (getf stats :incoming-tcp-connections))
                          (outgoing (getf stats :outgoing-tcp-connections))
                          (now (get-internal-real-time))
                          (sec (/ (- now last-time) internal-time-units-per-second))
                          (rate (/ (- finished-requests last-finished) sec)))
                     (setf last-finished finished-requests
                           last-time now)
                     (format t "incoming: ~a~%outgoing: ~a~%finished: ~a~%rate: ~f req/s~%~%" incoming outgoing finished-requests rate))
                   (unless quit
                     (as:delay #'show-stats :time 1))))
          (when stats (show-stats)))))
    :catch-app-errors t)
  (format t "TCP server exited.~%"))

;; run it
(tcp-server-test :stats t)
{% endhighlight %}

What's happening here is that the server gets a request, delays 5 seconds, then
responds on the same socket. This allows connections to build up for 5 seconds
before they start getting released, which is a good way to test how many
connections it can handle.

On another neighboring Linode, I ran
{% highlight bash %}
httperf --server=1.2.3.4 --port=9009 --num-conns=40000 --num-calls=10 --hog --rate=6000
{% endhighlight %}

In the [stats](/cl-async/stats#stats) output, I was getting:

    incoming: 12645
    outgoing: 0
    finished: 7330
    rate: 6026.183 req/s
    
So I was getting ~6000k req/s, and in some tests (longer delay value) I was able
to get the "incoming" connections to 40K. 6000/s seems to be the limit of the
machine `httperf` was running on, not the server, but I can't confirm this yet.
From the tests I ran, memory seems to be the number one constraining factor in
scalability of number of connections. The more memory, the more connections can
be handled.


