---
title: Futures | Documentation
layout: default
---

Futures
=======
A future is an object that may have a value at some point later on in the
execution of an application. 

- [Intro to futures](#intro)
- [Integration with cl-async](#integration)
- [future](#future) _class_
- [make-future](#make-future) _function_
- [set-event-handler](#set-event-handler) _function_
- [signal-event](#signal-event) _function_
- [futurep](#futurep) _function_
- [finish](#finish) _function_
- [attach-cb](#attach-cb) _function_
- [attach](#attach) _macro_


<a id="intro"></a>
Intro to futures
----------------
A future is a representation of a value in the (wait for it) future. The concept
is that you can attach actions to a future that will run once its value is
computed, and also attach an event handler to make sure any problems are handled
along the way.

Futures not only give an important abstraction for asynchronous programming, but
offer opportunities for syntactic abstraction that make async programming less
like [CPS](http://en.wikipedia.org/wiki/Continuation-passing_style).

Our implementation of futures are great for this because of the following
reasons:

- If a callback is [attached](#attach) to a value that is not a [future](#future),
that callback is called immediated with the value. This makes it so that you can
attach a callback to anything: a future or a value, and the end result is the
same. This way, the distiction between CPS and normal, stack-based programming
fades slightly because a function can return a future or a value, and you can
bind a callback to either.
- Calling [attach](#attach) always returns a future. This future get's fired
with the *return value of the callback being attached*. So if you have Future A
and you attach a callback to it, `attach` returns Future B. Future B gets
[finished](#finish) with the return value(s) from the callback attached to
Future A.
- [Finishing](#finish) a future with another future as the first value results
in the callbacks/event handler from the future being finished transferring over
to the future that is passed as the value. This, in addition to [attach](#attach)
always returning a future, makes nesting futures possible. In other words, a
future can result in a future which results in a future, and if the final future
is finished with a value, the callbacks attached to the first (top-level) future
will be called with this value. This provides what's almost a call stack for
asynchronous operations in that you can derive a value from deep within a bunch
of CPS calls back to the top-level, assuming that your operations are in the
tail position (remember, a callback has to return the future from the next
operation for this to work).

This is all probably greek, so let's give an example:

{% highlight cl %}
(defun future-calc (x)
  "Asynchronously add 1 to x, returning a future that will be finished when x is computed."
  (let ((future (as:make-future)))
    (as:delay (lambda () (as:finish future (+ x 1)))
              :time 1)
    future))

(as:start-event-loop
  (lambda ()
    (let ((future (as:attach (future-calc 0)
                    (lambda (x)           ;; x is 1 here
                      (as:attach (future-calc x)
                        (lambda (x)       ;; x is 2 here
                          (as:attach (future-calc x)
                            (lambda (x)   ;; x is 3 here
                              (* x 5)))))))))
      (as:attach future
        (lambda (x)
          (format t "Final result: ~a" x))))))
{% endhighlight %}

This waits 3 seconds then prints:

    Final result: 15

Notice how the callback was attached to the top-level future, but was able to
get the result computed from many async-levels deep. Not only does this mimick a
normal call stack a lot closer than CPS, but can be wrapped in macros that make
the syntax almost natural (note that these macros I speak of are on the way).

<a id="integration"></a>
Integration with cl-async
-------------------------
Futures are at their core, the representation of one value. Because of this, I
feel that their integration into cl-async is not appropriate. Most, if not all,
of the asynchronous operations in cl-async are stream-oriented, server oriented,
or have no values at all. The [http-client](/cl-async/http#http-client) is the
only piece that would benefit from using futures, and as such it's a bit weird
to use callbacks everywhere but one function, which uses futures.

Instead, futures are provided as a standard way to build drivers. Most drivers
provide a request-response interface, which is much more suited to futures.

So really, cl-async will at its core always use callbacks and CPS, but drivers
will be able to use futures to provide an interface that makes its users feel
less like they are programming javascript and more like programming lisp.

<a id="future-api"></a>
Futures API
----------
<a id="future"></a>
### future (class)
The future class represents a future value. For your application, it's mostly
an opaque object which can be operated on using the functions/macros below. It
currently has no public accessors, and mainly just holds callbacks, values,
events, etc.

The standard way to create a future is with [make-future](#make-future).

<a id="make-future"></a>
### make-future
Create a future. Supports persistent callbacks (can be fired more than once) and
reattaching callbacks to another future (when this future is [finished](#finish)
with another future as the value).

{% highlight cl %}
;; definition
(make-future &key preserve-callbacks (reattach-callbacks t))

;; example
(let ((future (make-future)))
  (attach future
    (lambda (x)
      (format t "x is ~a~%" x)))
  (finish future 5))
{% endhighlight %}

<a id="set-event-handler"></a>
### set-event-handler
Sets up a function to handle events/errors that happen on a future. Note that
events must be explicitely sent to a future via [signal-event](#signal-event).

When there is no event handler on a future, events are saved up until an event
handler is attached, at which point the events will be sent to the handler in
the order they were received.

{% highlight cl %}
;; definition
(set-event-handler future cb)

;; example
(let ((future (make-future))
      (socket (tcp-send "musio.com" 80 nil)))
  ;; set up our event handler
  (set-event-handler future
    (lambda (ev)
      (handler-case (error ev)
        (tcp-eof () (format t "peer closed socket.~%"))
        (tcp-timeout () (format t "connection timed out.~%"))
        (t () (format t "other event: ~a~%" ev)))))

  ;; write out our request
  (write-socket-data socket (format nil "GET /~c~c" #\return #\newline)
    :read-cb (lambda (sock data) (finish future data)))

  ;; attach a cb to our heroic future
  (attach future
    (lambda (data)
      (format t "got data: ~a~%" (babel:octets-to-string data)))))
{% endhighlight %}

<a id="signal-event"></a>
### signal-event
Signal an event on the future. Many async operations will signal events/errors,
and this allows you to "transfer" these events to a future. You can also [set up
an event handler](#set-event-handler) on the future to deal with these events.

{% highlight cl %}
;; definition
(signal-event future condition)

;; example
(let ((future (make-future)))
  ;; send out a request and finish our future when we get a response, but also
  forward any events get to the future to the handler can process them
  (tcp-send "musio.com" 80 (format nil "GET /~c~c" #\return #\newline)
    (lambda (sock data)
      (finish future data))
    (lambda (ev)
      ;; signal the event on the future
      (signal-event future ev)))

  ;; attach a callback to the tcp op
  (attach future
    (lambda (data)
      (format t "got data: ~a~%" (babel:octets-to-string data))))

  ;; handle any events
  (set-event-handler future
    (lambda (ev)
      (format t "ev: ~a~%" ev))))
{% endhighlight %}

<a id="futurep"></a>
### futurep
Test if the given object is a future.

{% highlight cl %}
;; definition
(futurep object)
{% endhighlight %}

<a id="finish"></a>
### finish
Finish a future with one or more values. When finished, all callbacks attached
to the future will be fired, with the given values as their arguments.

{% highlight cl %}
;; definition
(finish future &rest values)

;; example
(let ((future (make-future)))
  (as:delay (lambda () (finish future 1 2 3)))
  (attach future
    (lambda (x y z)
      (format t "result: ~a~%" (* x y z)))))
{% endhighlight %}

<a id="attach-cb"></a>
### attach-cb
This function attaches a callback to a future. _Please_ note that although there
are cases where calling this function directly are appropriate, the standard
interface for attaching callbacks to is [attach](#attach).

`attach-cb` takes two arguments, `future-values` and `cb`. `future-values` is a
list of values. If the first value in the list is a `future`, the given callback
is attached to that future to be fired when the future's value(s) are finished.
If the first item in `future-values` is *not* a `future` class, the _given
callback is fired instantly with the values passed as the arguments_.

The reason `attach-cb` fires the callback instantly is that it's sometimes nice
to attach a callback to a value when you don't know whether the value is a
future or an already-computed value. This allows for some nice syntactic
abstractions.

If `attach-cb` is called on a future that has already been finished, it fires
the given callback immediately with the future's value(s).

`attach-cb` returns one value: a future that is finished with the return values
of the given callback. So the original future fires, the callback gets called,
and then the future that was returned from `attach-cb` is fired with the return
values from the callback.

Also note that if a `future` is [finished](#finish) with another future as the
first value, the original future's callbacks/event handlers are _transfered_ to
the new future. This, on top of `attach-cb` always returning a future, makes
possible some incredible syntactic abstractions which can somewhat mimick non
CPS style by allowing the results from async operations several levels deep to
be viewable by the top-level caller.

<a id="attach"></a>
### attach
The attach macro is a thin wrapper around [attach-cb](#attach-cb) which allows
seamless capturing of multiple values.

It is also the standard interface for attaching callbacks to futures. [attach-cb](#attach-cb)
is a part of the public API, but its use should be limited by need.

{% highlight cl %}
;; definition (future-gen is an operation that may generate multiple values)
(attach future-gen callback)

;; example
(attach (my-async-op-which-returns-a-future)
  (lambda (x)
    (format t "x is ~a~%" x)))
{% endhighlight %}

