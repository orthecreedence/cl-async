---
title: Idlers | Documentation
layout: documentation
---

Idlers
===
An idler is an object that tells the event loop to call its callback once per
loop. This means your callback will be fired for each loop of the event loop
(including when the loop has events to process...don't be fooled into thinking
it will only be called when the loop is idle!)


- [idler](#idler) _class_
  - [idler-freed-p](#idler-freed-p) _method_
- [idle](#idle) _function_
- [free-idler](#free-idler) _function_

<a id="idler"></a>
### idler
This class wraps an idler, which fires your callback once per loop of the event
loop.

It is an opaque object meant to be passed to [free-idler](#free-idler).

<a id="idler-freed-p"></a>
##### idler-freed-p
{% highlight cl %}
(defmethod idler-freed-p ((idler idler)))
  => t/nil
{% endhighlight %}

This method lets us know if an idler has already been freed.

<a id="idle"></a>
### idle
{% highlight cl %}
(defun idle (callback &key event-cb))
  => idler
{% endhighlight %}

Starts calling the `callback` once per loop.

You can optionally pass an `:event-cb` function, called when errors occur while
running.

<a id="free-idler"></a>
### free-idler
{% highlight cl %}
(defun free-idler (idler))
  => nil
{% endhighlight %}

Stops the given idler and frees its resources.

