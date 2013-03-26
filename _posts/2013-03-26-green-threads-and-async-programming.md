---
title: Green threads and async programming
layout: post
---
Not too long ago, another lisper, [Stephen Goss](https://github.com/deliciousrobots),
wrote [a green threads library on top of cl-cont](https://github.com/deliciousrobots/green-threads).
It was recently converted to use cl-async's [futures](/cl-async/future), meaning
it can now be easily operated with any drivers built on the same futures
platform. It's now possible to write async code without using
[CPS](http://en.wikipedia.org/wiki/Continuation-passing_style).
Let's dive in for a few examples!

Let's create a synchronous delay (mimicking `sleep`). Note that `asf:` and `gt:`
are the cl-async-future and green-threads package nicknames, respectively:

{% highlight cl %}
;; we *must* use defun/cc or without-call/cc anywhere gt:wait-on is used,
;; otherwise wait-on will not function properly.
(cl-cont:defun/cc fdelay (value &key (time 1))
  (let ((future (asf:make-future)))
    (as:delay (lambda () (finish future value))
              :time time)
    ;; this call blocks this green thread (assuming it's wrapped in one of
    ;; cl-cont's macros) while allowing the event loop to process more events.
    ;; once the future is finished, this green thread continues execution and
    ;; returns the values the future was finished with.
    (gt:wait-on future)))

(as:start-event-loop
  (lambda ()
    ;; start a green thread
    (gt:with-green-thread
      (format t "Start.~%")
      (format t "End: ~a.~%" (fdelay 5)))))
{% endhighlight %}

In the above example, our fdelay function creates a future, spawns a delay to
finish it at the given time, then yields the current green thread by using the
`wait-on` method. `wait-on` takes a future, and once that future is finished,
continues execution of the thread, returning the values the future was finished
with.

The output:

    Start.
    ;; 1s delay
    End: 5

Note how where we'd normally have to put the second `(format)` call into a
callback function, we can now keep it in the same stack as the first `format`.
In other words, by using green-threads along with cl-cont, we've eliminated CPS!

As noted in the comments, it's important to mention that any time `wait-on` is
called, it must be wrapped in a cl-cont macro: `without-call/cc`. `defun/cc` and
`with-green-thread` both do this for you. This macro transforms the code that
appears to be in the same stack (like the `(format ...)` statements in the
example) into CPS. So we're not creating "real" stacks/threads and yielding
them, we're using cl-cont to make it appear as though we are, and green-threads
is very tactfully managing all the state for us.

We can do more than just delay though, we can use a library like [drakma-async](https://github.com/orthecreedence/drakma-async)
to "synchronously" grab a webpage:

{% highlight cl %}
(as:start-event-loop
  (lambda ()
    (gt:with-green-thread
      (format t "Google's homepage: ~a~%"
                (gt:wait-on (drakma-async:http-request "http://google.com"))))))
{% endhighlight %}

Here, we asynchronously grab a web page (cl-async continues to process events
while we're waiting on `http-request`), and pass the result to `format` as if
it's a synchronous call.

With the absence of real continuations, coroutines, and green threads in lisp,
it can be very difficult to give asynchronous programming a natural syntax,
because by nature it's all CPS. Using cl-cont and green-threads shrinks this gap
significantly.

It's important to note the limitations of this method. The main one is error and
condition handling. What looks like a normal stack is actually being transformed
into CPS, so catching and handling errors becomes suddenly more difficult. On
top of this, the [future-handler-case](/cl-async/future#future-handler-case)
macro won't do much when using green-threads' `wait-on` because it doesn't know
to look for it. It might make sense to make `wait-on` a macro that expands to an
`attach` call so that errors can be handled more effectively.

Hopefully you now have a better understanding of how [green-threads](https://github.com/deliciousrobots/green-threads)
is a way to transform ugly CPS-style code into normal stack-based code. It's not
a perfect solution to the age-old problem, but it works well and, in my opinion,
is a pretty badass project worth trying out...and because it now integrates with
[cl-async-future](https://github.com/orthecreedence/cl-async-future), it can be
used very easily alongside any of your async programming.
