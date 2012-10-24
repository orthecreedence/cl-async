---
title: Statistics
layout: default
---

Stastistics
===========

- [stats](#stats) _function_

<a id="stats"></a>
### stats
This function returns data on the current state of the cl-async internals. How
many incoming/outgoing connections, how many registered callbacks, how many
registered data objects, how many open DNS requests, etc.

Data is a plist. Stats might change in the near future.

{% highlight cl %}
;; definition
(stats)  =>  
  '(:open-dns-queries 3
    :fn-registry-count 418
	:data-registry-count 416
	:incoming-tcp-connections 390
	:outgoing-tcp-connections 28
	:incoming-http-connections 0
	:outgoing-http-connections 0)
{% endhighlight %}


