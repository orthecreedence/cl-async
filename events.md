---
title: Events | Documentation
layout: documentation
---

<a id="timers"></a>
Events
======
This section goes over basic events in cl-async and how to use them for various
purposes, such as triggering a function after a specific amount of time, or
watching an OS file descriptor for changes.

- [event](#event) _class_
  - [event-c](#event-c) _accessor_
  - [event-freed-p](#event-freed-p) _method_
- [free-event](#free-event) _function_
- [remove-event](#remove-event) _function_
- [add-event](#add-event) _function_
- [delay](#delay) _function_
- [with-delay](#with-delay) _macro_
- [interval](#interval) _function_
- [with-interval](#with-interval) _macro_
- [remove-interval](#remove-interval) _function_
- [make-event](#make-event) _function_
- [watch-fd](#watch-fd) _function_
- [event-freed](#event-freed) _condition_
- [ref](#ref) _function_
- [unref](#unref) _function_

<a id="event"></a>
### event
The event class wraps around a libevent event C object. It is accepted by
[free-event](#free-event), [remove-event](#remove-event), and [add-event](#add-event).

<a id="event-c"></a>
##### event-c
Allows you to access the underlying C libevent object associated with this event.
This is for use by advanced users who want more control over the event, and have
knowledge of how libevent works.

It's exported in case you need to do something cl-async doesn't provide.

<a id="event-freed-p"></a>
##### event-freed-p
Returns `t` if an event is already freed (via [free-event](#free-event)), and
`nil` if it has not been freed. Trying to operate on an event that has been
freed will result in an [event-freed](#event-freed) error.

<a id="free-event"></a>
### free-event
{% highlight cl %}
(defun free-event (event))
  => nil
{% endhighlight %}

Frees an event's underlying event object, and performs any needed cleanup. It
is *absolutely safe* to free an event that is pending (watched by the event loop)
or active. Doing so makes the event inactive/non-pending before freeing.

<a id="remove-event"></a>
### remove-event
{% highlight cl %}
(defun remove-event (event))
  => t/nil
{% endhighlight %}

This removes an event from the event loop. The event still exists, and can be
added back into the event loop via [add-event](#add-event), but must be
activated again after adding back into the loop (by either specifying
`:active t` or giving a timeout in seconds via `:timeout`.

Returns `t` on success, `nil` otherwise (a `nil` can mean that the event is
already removed and doesn't need to be remove again).

<a id="add-event"></a>
### add-event
{% highlight cl %}
(defun add-event (event &key timeout activate)
  => nil
{% endhighlight %}

Makes an event pending (adds it to the event loop it was created with). This is
mainly done to resume an event that was removed from the event loop using
[remove-event](#remove-event).

If `:timeout` is specified, the event will fire in the given number of seconds
regardless of whether or not the other conditions the event is watching for are
met.

If `:activate` is true *and* `:timeout` is null, the event is not added back
into the event loop, but is instead manually activated (via [libevent's
event\_active function](http://www.wangafu.net/~nickm/libevent-book/Ref4_event.html#_manually_activating_an_event)).

Note that if you omit *both* `:activate` *and* `:timeout`, the event will be
added to the loop but not activated (and will never be run unless you call
`add-event` again with either `:activate` or `:timeout` specified).

<a id="delay"></a>
### delay
{% highlight cl %}
(defun delay (callback &key time event-cb))
  => event
{% endhighlight %}

Run a function asynchronously. Takes two optional parameters: `time`, the number
of seconds to wait before running the given function (run with no delay if
`nil`), and `event-cb` which can be used to catch application errors should they
occur while running `callback`.

{% highlight cl %}
;; example:
(delay (lambda () (format t "Run me immediately after control is given to the event loop.~%")))
(delay (lambda () (format t "I will run 3.2 seconds after calling (delay).~%")) :time 3.2)
{% endhighlight %}

<a id="with-delay"></a>
### with-delay
{% highlight cl %}
(defmacro with-delay ((seconds) &body body))
  => event
{% endhighlight %}

Syntax wrapper around [delay](#delay) to make it a bit less annoying to type.

{% highlight cl %}
;; example:
(with-delay (5)
  (format t "Five seconds passed!~%"))
{% endhighlight %}

<a id="interval"></a>
### interval
{% highlight cl %}
(defun interval (callback &key time event-cb))
  => cancel-closure
{% endhighlight %}

Like [delay](#delay), but runs the given callback every `time` seconds until
stopped by either `funcall`ing the returned closure, or calling [remove-inteval](#remove-interval)
on the returned closure.

<a id="with-interval"></a>
### with-interval
{% highlight cl %}
(defmacro with-interval ((seconds) &body body))
  => cancel-closure
{% endhighlight %}

Syntax wrapper around [interval](#interval). Returns the same cancellation
closure that `interval` does.

<a id="remove-interval"></a>
### remove-interval
{% highlight cl %}
(defun remove-interval (interval-closure))
  => nil
{% endhighlight %}

When given the closure returned from calling [interval](#interval), stops the
timer on the interval (cancels it).

<a id="make-event"></a>
### make-event
{% highlight cl %}
(defun make-event (callback &key event-cb))
  => event
{% endhighlight %}

Makes it easy to add an arbitrary event to the event loop. This event needs to
either be activated via `(add-event event :activate t)` (see [add-event](#add-event))
or removed manually.

If you use `enable-threading-support`, you can activate the event from another
thread. This is really useful if you want to queue long-running work in a
background thread. Then once the work is done you call `add-event` on the event
and libevent will trigger the callback for you.

This is a very thin wrapper around [delay](#delay), in fact all it does is call
`delay` with a one-year delay.

<a id="watch-fd"></a>
### watch-fd
{% highlight cl %}
(defun watch-fd (fd &key event-cb read-cb write-cb timeout-cb timeout))
  => event
{% endhighlight %}

Allows you to create an event that watches a file descriptor. This can be useful
if you need to manually watch a socket without having libevent/cl-async do any
buffering on it.

`:read-cb` is fired when the fd is ready to be read from.  
`:write-cb` is fired when the fd is ready to be written to.  
`:timeout-cb` is fired when the time given by `:timeout` (in seconds) expires.  
`:timeout` specifies that the event fire in the given number of seconds.

cl-async does not provide any abstractions to get a file descriptor from a
socket or other objects that may hold one, it is your responsibility to get
this fd yourself, and pass it to `watch-fd`.

__NOTE__: `watch-fd` does *not* free the event it creates for you. It is up to
you to track the event it returns and call [free-event](#free-event) on it
manually when you no longer need the event.

<a id="event-freed"></a>
### event-freed
_extends [event-error](/cl-async/base#event-error)_

This error is thrown when an event that has been freed is operated on in some
way. You can test if an event is freed already using the [event-freed-p](#event-freed-p)
method.

<a id="ref"></a>
### ref
{% highlight cl %}
(defmethod ref ((handle event)))
  => nil
{% endhighlight %}

References the event, making it so that the event loop will not exit until the
event is freed.

See [unref](#unref) as well.

<a id="unref"></a>
### unref
{% highlight cl %}
(defmethod unref ((handle event)))
  => nil
{% endhighlight %}

Unreferences the event. This means that even if it's active in the event loop,
the loop can exit without the event being closed/freed.

See [ref](#ref) as well.

