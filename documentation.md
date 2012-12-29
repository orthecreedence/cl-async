---
title: Documentation
layout: documentation
---

<a id="documentation"></a>
Documentation
=============
The documentation is split into a number of sections covering the various
abilities of cl-async:

- API Documentation
  - [Base system](/cl-async/base)
  - [Timers](/cl-async/timers)
  - [Signal handling](/cl-async/signal-handling)
  - [DNS](/cl-async/dns)
  - [TCP](/cl-async/tcp)
  - [TCP stream](/cl-async/tcp-stream)
  - [TCP SSL](/cl-async/tcp-ssl)
  - [HTTP](/cl-async/http) &nbsp;&nbsp;<small>*DEPRECATED*</small>
  - [Futures](/cl-async/future)
  - [Stats](/cl-async/stats)
- [Event callbacks and error handling](/cl-async/event-handling)

<a id="quick-start"></a>
Quick start
-----------
Cl-async uses an event loop (via [libevent](http://libevent.org/)) to keep track
of events it's currently processing. An event can be something like a timer
being fired, data being received on a socket, an operating system signal being
delivered to your application, etc. Everything that an evented application does
is in response to an event being processed.

The first thing to do in cl-async is to start the event loop:

{% highlight cl %}
(as:start-event-loop
  (lambda ()
    (format t "Exiting event loop.~%")))
{% endhighlight %}

The event loop returns immediately after printing `Exiting event loop.` because
we did not give it any events to process before returning control back. So in
the next example, let's give it an event to process:

{% highlight cl %}
(as:start-event-loop
  (lambda ()
    (as:delay
      (lambda ()
        (format t "Timer fired. Exiting.~%"))
      :time 3)))
{% endhighlight %}

What we're doing here is adding a function to be run on a timer (in this case,
3 seconds). Once this is done, control is return to the event loop, which will
not exit yet because we just put an event on it, and event loops do not exit
until all events are processed. Once the 3 seconds is up, the function we gave
to `delay` will be run. After printing `Timer fired. Exiting.`, the event loop
exits to the REPL because there are no more events left to process.

You may be wondering, why not just use `(sleep 2)`. In this case, using sleep
would work the same. The power of an event loop is that while `(sleep)` would
block the execution of the current thread, `(delay)` will not...it returns
immediately after being called, no longer how long of a `:time` you specify.
This means that while the delay event is sitting in the event loop waiting for
its timer to count down, your app is free to do other things.

Using this methodology, your application can processing tens if not hundreds of
thousands of strings of execution concurrently, all while only using one thread.

[See more cl-async examples to get you started](/cl-async/examples).

<a id="performance"></a>
Event loop performance
----------------------
It's important to note what event loops are good for and what they are bad for.
Event loops are wonderful if 50% or more of what you're doing is input/output
(IO), since most of the time your app is just waiting, which allows you to
process many things at once. If your app is doing mostly CPU work, an event loop
is probably going to give you worse performance than just using threading.

So if you're running a server, an event loop might be good because you can
handle tens of thousands of clients at the same time. If you're folding protiens
or something similar, just use threading.


