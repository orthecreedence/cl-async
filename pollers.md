---
title: Pollers | Documentation
layout: documentation
---

Pollers
===
A poller is an object that watches an operating system file descriptor for you
and tells you when it's ready to be read or written to.


- [poller](#poller) _class_
  - [poller-freed-p](#poller-freed-p) _method_
- [poll](#poll) _function_
- [free-poller](#free-poller) _function_

<a id="poller"></a>
### poller
This class wraps a poller, responsible for notifying you of changes to a file
descriptor.

It is an opaque object meant to be passed to [free-poller](#free-poller).

<a id="poller-freed-p"></a>
##### poller-freed-p
{% highlight cl %}
(defmethod poller-freed-p ((poller poller)))
  => t/nil
{% endhighlight %}

This method lets us know if a poller has already been freed.

<a id="poll"></a>
### poll
{% highlight cl %}
(defun poll (fd poll-cb &key event-cb (poll-for '(:readable :writable)) socket))
  => poller
{% endhighlight %}

Starts polling the given `fd`, calling the `poll-cb` callback whenever the `fd`
is ready for the actions given in the list `:poll-for` (allowed actions are
`:readable` and `:writable`).

You can optionally pass an `:event-cb` function, called when errors occur while
polling.

If you are polling an fd that's a socket, you __must__ pass `:socket t`.

<a id="free-poller"></a>
### free-poller
{% highlight cl %}
(defun free-poller (poller))
  => nil
{% endhighlight %}

Stops the given poller and frees its resources.

