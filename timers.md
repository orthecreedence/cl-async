---
title: Timers | Documentation
layout: default
---

<a id="timers"></a>
Timers/delayed functions
========================

- [delay](#delay) _function_
- [timer](#timer) _function (deprecated)_

<a id="delay"></a>
### delay
Run a function asynchronously. Takes two optional parameters: `time`, the number
of seconds to wait before running the given function (run with no delay if
`nil`), and `event-cb` which can be used to catch application errors should they
occur while running `callback`.

{% highlight cl %}
;; definition:
(delay callback &key time event-cb)

;; example:
(delay (lambda () (format t "Run me immediately after control is given to the event loop.~%")))
(delay (lambda () (format t "I will run 3.2 seconds after calling (delay).~%")) :time 3.2)
{% endhighlight %}

<a id="timer"></a>
### timer
_Deprecated_

Yes, even in its infancy, this library has a deprecated function. Use
[delay](#delay) for running functions asynchronously!

