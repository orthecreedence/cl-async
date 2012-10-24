---
title: Examples
layout: default
---

Examples
========

Some limited examples are given in the [documentation](/cl-async/documentation),
but more in-depth example usages are a great way to learn. This section will
give some basic usage examples to get you started, and you can view more
complicated apps in [the examples/ folder on github](https://github.com/orthecreedence/cl-async/tree/master/examples).

<a id="echo-server"></a>
### An echo server

{% highlight cl %}
(defun my-echo-server ()
  (format t "Starting server.~%")
  (as:tcp-server nil 9003  ; nil is "0.0.0.0"
                 (lambda (socket data)
                   ;; echo the data back into the socket
                   (as:write-socket-data socket data))
                 (lambda (err) (format t "listener event: ~a~%" err)))
  ;; catch sigint
  (as:signal-handler 2 (lambda (sig)
                         (declare (ignore sig))
                         (as:exit-event-loop))))

(as:start-event-loop #'my-echo-server)
{% endhighlight %}

This listens to "0.0.0.0" on port 9003. When any data becomes available on the
server, it immediately echos that data back to the connecting client. This goes
on ad infinitum until the server recieves a SIGINT signal, then it forcibly
exits the event loop.


... more examples on the way ...
