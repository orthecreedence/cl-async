---
title: Event callbacks and error handling | Documentation
layout: default
---

Event callbacks (and error handling in general)
===============================================

Any parameter labelled `event-cb` is what's known as an "event callback." Event
callbacks have one argument: a condition describing the event that caused them
to be invoked. Originally event callbacks were failure callbacks, but since
non-failure conditions are sometimes useful to an app, it made sense to make it
more generic.

The event conditions generally match conditions in libevent, although they try
to be as informative as possible. Note that conditions are not actually thrown,
but rather instantiated via `make-instance` and passed directly to the event
callback.

<a id="application-error-handler"></a>
### Application error handling
cl-async can be set up to catch errors in your application and pass them to
your `event-cb`. This makes for seamless error handling, and keeps a rogue
condition from exiting the event loop (assuming you have an `event-cb` set for
the operation that generated the condition, or a default event handler that
deals with the condition).

Note that the following variables are also controllable on a per-event-loop
basis via the [start-event-loop](/cl-async/base#start-event-loop) keyword arguments
`:catch-app-errors` and `:default-event-cb`. It might actually be favorable to
use [start-event-loop](/cl-async/base#start-event-loop) since it creates thread-local versions
of these variables when instantiating, which can be useful if running event
loops in multiple threads.

<a id="catch-application-errors"></a>
##### \*catch-application-errors\*
_default: `nil`_

By setting this to true, you allow cl-async to catch conditions in your app and
pass them to the event callback associated with the procedure that triggered the
condition.

If this is left as `nil`, triggered conditions will make their way to the top
level and cause the event loop to exit, cancelling any pending events (unless
you have restarts implemented in your app).

<a id="default-event-handler"></a>
##### \*default-event-handler\*
When [\*catch-application-errors\*](#catch-application-errors) is set to `t`
and an `event-cb` is not specified for an operation, the function assigned to
this variable will be used as the `event-cb`. The default:

{% highlight cl %}
(lambda (err)
  ;; throw the error so we can wrap it in a handler-case
  (handler-case (error err)
    ;; got a connection error, throw it (must do this explicitely since
    ;; connection-error extends connection-info)
    (connection-error () (error err))

    ;; this is just info, let it slide
    (connection-info () nil)

    ;; this an actual error. throw it back to toplevel
    (t () (error err))))
{% endhighlight %}

This can be changed by your application if different behavior is desired.


