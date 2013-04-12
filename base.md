---
title: Base system | Documentation
layout: documentation
---

<a id="base-system"></a>
Base system
===========
This section describes the base functions for cl-async and also the low-level
conditions it uses.

- [start-event-loop](#start-event-loop) _function_
- [with-event-loop](#with-event-loop) _macro_
- [exit-event-loop](#exit-event-loop) _function_
- [add-event-loop-exit-callback](#add-event-loop-exit-callback) _function_
- [dump-event-loop-status](#dump-event-loop-status) _function_
- [event-info](#event-info) _condition_
- [event-error](#event-error) _condition_
  - [event-errcode](#event-errcode) _accessor_
  - [event-errmsg](#event-errmsg) _accessor_

<a id="start-event-loop"></a>
### start-event-loop
{% highlight cl %}
(defun start-event-loop (start-fn &key fatal-cb logger-cb default-event-cb catch-app-errors))
  => integer
{% endhighlight %}

Start the event loop, giving a function that will be run inside the event loop
once started. `start-event-loop` blocks the main thread until the event loop
returns, which doesn't happen until the loop is empty *or*
[exit-event-loop](#exit-event-loop) is called inside the loop.

This function must be called before any other operations in the library are
allowed. If you try to do an async operation without an event loop running, it
will throw an error.

It allows specifying callbacks for the fatal errors in libevent (called when
libevent would normally exit, taking your app with it), logging, and default
application error handling.

`start-event-loop` returns 1 if the event loop exited naturally, and 0 if it was
forced to close via [exit-event-loop](#exit-event-loop).

{% highlight cl %}
;; example:
(start-event-loop (lambda () (format t "Event loop started.~%")))
{% endhighlight %}

<a id="fatal-cb-definition"></a>
##### fatal-cb definition

{% highlight cl %}
(lambda (errcode) ...)
{% endhighlight %}

<a id="logger-cb-definition"></a>
##### logger-cb definition

{% highlight cl %}
(lambda (loglevel msg) ...)
{% endhighlight %}

`loglevel` corresponds to syslog levels.

<a id="default-event-cb"></a>
##### default-event-cb and catch-app-errors
Please see the [application error handling](/cl-async/event-handling#application-error-handling)
section for complete information on these. They correspond 1 to 1 with
[\*default-event-handler\*](/cl-async/event-handling#default-event-handler) and
[\*catch-application-errors\*](/cl-async/event-handling#catch-application-errors). Setting them when
calling `start-event-loop` not only cuts down on `setf`s you have to do when
starting your evented app, but also uses thread-local versions of the vars,
meaning you can start multiple event loops in multiple threads wiithout using
the same values for each thread.

<a id="with-event-loop"></a>
### with-event-loop
{% highlight cl %}
(defmacro with-event-loop ((&key fatal-cb logger-cb default-event-cb (catch-app-errors nil catch-app-errors-supplied-p))
                           &body body)
  => integer
{% endhighlight %}

Wraps around [start-event-loop](#start-event-loop), taking away a little bit of
the syntax. The options match up 1 to 1 with `start-event-loop`, so I won't
duplicate the documentation.

<a id="exit-event-loop"></a>
### exit-event-loop
{% highlight cl %}
(defun exit-event-loop ())
  => nil
{% endhighlight %}

Exit the event loop. This will free up all resources internally and close down
the event loop.

Note that this doesn't let queued events process, and is the equivelent of
doing a force close. Unless you really need to do this and return control to
lisp, try to let your event loop exit of "natural causes" (ie, no events left to
process). You can do this by freeing your signal handlers, servers, etc. This
has the added benefit of letting any connected clients finish their requests
(without accepting new ones) without completely cutting them off.

<a id="add-event-loop-exit-callback"></a>
### add-event-loop-exit-callback
{% highlight cl %}
(defun add-event-loop-exit-callback (fn))
  => nil
{% endhighlight %}

Adds a callback to the event loop that will be fired when it exits. The callback
takes no arguments. It can be used to clean up/alert various sections of your
code that would benefit from knowing that an event loop has exited (generally
you can just put this cleanup directly after calling [start-event-loop](#start-event-loop),
but it can make sense to pass in closures that do cleanup for you.

<a id="dump-event-loop-status"></a>
### dump-event-loop-status
{% highlight cl %}
(defun dump-event-loop-status (file &key (return-as-string t)))
  => string
{% endhighlight %}

This is a debugging function that can help shed *some* light on why an event
loop isn't exiting by dumping out all the events the loop currently holds.
Although it's not very specific about the events it contains, it can still be
useful.

Since libevent only lets you dump the events to a file, the default is for you
to specify which file to dump into, which is then read back as a string (and the
file is then removed). If you wish the keep the file, pass
`:return-as-string nil`.

<a id="event-info"></a>
### event-info
Base event. Signals that "something" happened. Meant to be extended.

<a id="event-error"></a>
### event-error
_extends [event-info](#event-info)_

Base error event. Signals that "something bad" (an error) happened.

<a id="event-errcode"></a>
##### event-errcode
The error code associated with the error event. This is generally retrieved
from the underlying OS, but sometimes cl-async will generate its own error
conditions, in which case errcode will be -1.

<a id="event-errmsg"></a>
##### event-errmsg
Much like `event-errcode`, this is generally a system message explaining the
error. If it is a cl-async generated error, it will have a string value
explaining what happened.

