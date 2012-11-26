---
title: Examples
layout: default
---

Examples
========
Some limited examples are given in the [documentation](/cl-async/documentation),
but more in-depth example usages are a great way to learn. This section will
give some basic usage examples to get you started.

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

<a id="future-example"></a>
### Using futures (a simple driver example)
For this example, make sure the `cl-async-future` package is loaded. We're going
to create a simple client for an imaginary HTTP server that returns information
on a user.

{% highlight cl %}
(define-condition server-error (error)
  ((status :initarg :code :accessor server-error-status :initform nil)))

(defun get-user-from-server (id)
  "Spawn an HTTP request which grabs the given user by id. When the response
   comes in, finish the returned future with the parsed JSON of the response."
  (let ((future (make-future)))
    (as:http-client (format nil "http://www.test-server.com/users/~a" id)
      (lambda (status headers body)
        (if (<= 200 status 299)
            ;; success. note we finish the future with multiple values (parsed json + status code)
            (finish future (yason:parse body) status)
            ;; error code, signal error
            (signal-error future (make-instance 'server-error :code status)))
        (finish future status headers body))
      (lambda (event)
        (let ((event-type (type-of event)))
          ;; ignore info events
          (when (or (not (subtypep event-type 'as:connection-info))
                    (subtypep event-type 'as:connection-error))
            (signal-error future event))))
      :timeout 5)
    future))

(defun my-get-user (id)
  (future-handler-case
    ;; bind multiple values to the future returned from get-user-from-server
    (multiple-future-bind (user-data status)
        (get-user-from-server id)  ; make the request
      (format t "Got user: ~a~%" (gethash "name" user-data))
      status)
    (server-error (e)
      (format t "Error while getting user: status code (~a)~%" (server-error-status e)))
    (t (e)
      (format t "Got general error while looking up user: ~a~%" e))))

(as:start-event-loop (lambda () (my-get-user 17)) :catch-app-errors t)
{% endhighlight %}

This is an example of a very simple driver built on top of futures. For a real
driver, the `get-user-from-server` would most likely be a lot more general and
would eb able to process a number of different driver commands, but for this
example it works fine the way it is.

So what happens is `get-user-from-server` spawns an HTTP request and returns a
future. The future is bound via `multiple-future-bind`, whos body fires once the
future is finished with the response.

In the case of an error, `future-handler-case` hooks into the future's error
mechanism and catches any errors triggered via `signal-error`. Thus, the markup
can be surprisingly like normal lisp but everything operates asynchronously. The
only difference is that the `my-get-user` function will not return `status` as
it would in normal lisp, but instead will return a future that will finish with
the value of `status` once the body of `multiple-future-bind` returns. So if we
want the value of status in the top-level form, we can do:

{% highlight cl %}
(as:start-event-loop
  (lambda ()
    (alet ((status (my-get-user 17)))
      (format t "final status: ~a~%" status)))
  :catch-app-errors t)
{% endhighlight %}

This is very close to normal lisp syntax, except that anything that returns a
future must be wrapped in [attach](/cl-async/future#attach) or
[some kind of future syntax macro](/cl-async/future#nicer-syntax) to work as
expected.

This is the standard way to implement [drivers](/cl-async/drivers) in cl-async.
Read [more on futures](/cl-async-future) to get an understanding of what's going
on.

Github examples
---------------
See [the examples on github](https://github.com/orthecreedence/cl-async/tree/master/examples),
which provide some more advanced code samples.
