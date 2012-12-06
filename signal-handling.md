---
title: Signal handling | Documentation
layout: default
---

Signal handling
===============
This section goes over how to tie into your operating system's signaling system
so your application can listen to the signals passed to it and act accordingly.

- [signal-handler](#signal-handler) _function_
- [free-signal-handler](#free-signal-handler) _function_
- [clear-signal-handlers](#clear-signal-handlers) _function_
- [signal definitions](#signal-definitions)

<a id="signal-handler"></a>
### signal-handler
{% highlight cl %}
(defun signal-handler (signo signal-cb &key event-cb))
  => nil
{% endhighlight %}

Create a signal handler. This listens for the given `signo` not only in the 
event loop, but also in the lisp app as well. It replaces the current lisp
signal handler by calling C's `signal` function. When a signal handler is freed
via [free-signal-handler](#free-signal-handler), the original lisp signal
handler is restored as it was before binding the signal handler.

Note that signals that aren't freed via [free-signal-handler](#free-signal-handler)
or [clear-signal-handlers](#clear-signal-handlers) will linger on even after all
other events are out of the event loop, which prevents it from exiting. If you
want your event loop to exit naturally, you must free your signals when you're
done with them.

{% highlight cl %}
;; example
(signal-handler 2 (lambda (sig) (format t "got SIGINT: ~a~%" sig))
                  (lambda (err) (foramt t "error processing signal callback: ~a~%" err)))
{% endhighlight %}

The `signo` arg is the POSIX integer signal you want to handle.

In the case of `signal-handler`, `event-cb` will *only* be called when an error
occurs in the signal callback. There are no cl-async events that occur during
signal processing.

<a id="signal-handler-signal-cb"></a>
##### signal-cb definition
{% highlight cl %}
(lambda (signo) ...)
{% endhighlight %}

<a id="free-signal-handler"></a>
### free-signal-handler
{% highlight cl %}
(defun free-signal-handler (signo))
  => nil
{% endhighlight %}

Unbinds a signal handler. This deletes the libevent signal listener event and
also restores the lisp signal handler that existed before calling
[signal-handler](#signal-handler).

{% highlight cl %}
;; example
(signal-handler 2
  (lambda (sig)
    (close-server *my-app-server*)
    (free-signal-handler 2)))
{% endhighlight %}

<a id="clear-signal-handlers"></a>
### clear-signal-handlers
{% highlight cl %}
(defun clear-signal-handlers ())
  => nil
{% endhighlight %}

Clear all cl-async bound signal handlers. This deletes the libevent event
listeners and restores the original lisp signal handlers for each bound signal.

This is useful if you don't want to track all the signals you've bound and
[free](#free-signal-handler) them manually, but don't want to [exit the event
loop forcibly](/cl-async/base#exit-event-loop).

{% highlight cl %}
{% endhighlight %}

<a id="signal-definitions"></a>
### Signal definitions
These signals correspond directly to the [unix signals](http://unixhelp.ed.ac.uk/CGI/man-cgi?signal+7):

- `+sighup+`
- `+sigint+`
- `+sigquit+`
- `+sigill+`
- `+sigtrap+`
- `+sigabrt+`
- `+sigemt+`
- `+sigfpe+`
- `+sigkill+`
- `+sigbus+`
- `+sigsegv+`
- `+sigsys+`
- `+sigpipe+`
- `+sigalrm+`
- `+sigterm+`
- `+sigurg+`
- `+sigstop+`
- `+sigtstp+`
- `+sigcont+`
- `+sigchld+`
- `+sigttin+`
- `+sigttou+`
- `+sigio+`
- `+sigxcpu+`
- `+sigxfsz+`
- `+sigvtalrm+`
- `+sigprof+`
- `+sigwinch+`
- `+siginfo+`
- `+sigusr1+`
- `+sigusr2+`
