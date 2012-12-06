---
title: Statistics
layout: default
---

Stastistics
===========
Cl-async keeps internal records of what's going on inside of it. This section
goes over how to peek into this data for a current snapshot of what's going on
in your app.

- [stats](#stats) _function_

<a id="stats"></a>
### stats
{% highlight cl %}
(defun stats ())
  => plist
{% endhighlight %}

This function returns data on the current state of the cl-async internals. How
many incoming/outgoing connections, how many registered callbacks, how many
registered data objects, how many open DNS requests, etc.

Data is a plist. Stats might change in the near future.

{% highlight cl %}
;; example output
(stats)  =>  
  '(:open-dns-queries 3
    :fn-registry-count 418
	:data-registry-count 416
	:incoming-tcp-connections 390
	:outgoing-tcp-connections 28
	:incoming-http-connections 0
	:outgoing-http-connections 0)
{% endhighlight %}


