---
title: Notifiers | Documentation
layout: documentation
---

Notifiers
===
An idler is an object that tells the event loop to call its callback once per
loop. This means your callback will be fired for each loop of the event loop
(including when the loop has events to process...don't be fooled into thinking
it will only be called when the loop is idle!)


- [notifier](#notifier) _class_
  - [notifier-freed-p](#notifier-freed-p)
- [make-notifier](#make-notifier) _function_
- [trigger-notifier](#trigger-notifier) _function_
- [free-notifier](#free-notifier) _function_
- [ref](#ref) _function_
- [unref](#unref) _function_

<a id="notifier"></a>
### notifier
This class wraps a notifier, an object which lets you trigger a callback that
runs in your event loop *from another thread*.

It is an opaque object meant to be passed to [trigger-notifier](#trigger-notifier)
or [free-notifier](#free-notifier).

<a id="notifier-freed-p"></a>
##### notifier-freed-p
{% highlight cl %}
(defmethod notifier-freed-p ((notifier notifier)))
  => t/nil
{% endhighlight %}

This method lets us know if an notifier has already been freed.

<a id="make-notifier"></a>
### make-notifier
{% highlight cl %}
(defun make-notifier (callback &key event-cb (single-shot t)))
  => notifier
{% endhighlight %}

Returns a notifier object, which can be given to [trigger-notifier](#trigger-notifier)
from *any thread* to trigger `callback` in the current event loop. This allows
you to safely run code in your event loop that's triggered by some event or
action in another thread.

Note that the notifier is by default a single-shot, meaning once it is triggered
it frees itself. This behavior can be changed bas passing `:single-shot nil`.

You can optionally pass an `:event-cb` function, called when errors occur while
running.

<a id="trigger-notifier"></a>
### trigger-notifier
{% highlight cl %}
(defun trigger-notifier (notifier))
  => nil
{% endhighlight %}

Triggers the given notifier. This can be called from any thread, and will cause
the event loop to invoke the notifier's callback *in the event loop thread*.

<a id="free-notifier"></a>
### free-notifier
{% highlight cl %}
(defun free-notifier (notifier))
  => nil
{% endhighlight %}

Stops the given notifier and frees its resources.

<a id="ref"></a>
### ref
{% highlight cl %}
(defmethod ref ((handle notifier)))
  => nil
{% endhighlight %}

References the notifier, making it so that the event loop will not exit until
the notifier is freed.

See [unref](#unref) as well.

<a id="unref"></a>
### unref
{% highlight cl %}
(defmethod unref ((handle notifier)))
  => nil
{% endhighlight %}

Unreferences the notifier. This means that even if it's active in the event
loop, the loop can exit without closing the notifier. This allows you to have
open notifiers in other threads *without* blocking your event loop from closing.

See [ref](#ref) as well.

