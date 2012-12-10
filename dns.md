---
title: DNS | Documentation
layout: documentation
---

DNS
===
This section details resolving hostnames to addresses, as well as
conditions/events that occur while using the DNS system.

- [dns-lookup](#dns-lookup) _function_
- [start-dns-logging](#start-dns-logging) _function_
- [stop-dns-logging](#stop-dns-logging) _function_
- [dns-error](#dns-error) _condition_

<a id="dns-lookup"></a>
### dns-lookup
{% highlight cl %}
(defun dns-lookup (host resolve-cb event-cb &key (family +af-unspec+)))
  => nil
{% endhighlight %}

Asynchronously lookup an IP address given a hostname. If the hostname is an IP
address already, the mechanics are the same although the callback is called
synchronously.

The `:family` keyword can be one of `+af-inet+`, `+af-inet6+`, `+af-unspec+`.

{% highlight cl %}
;; example
(dns-lookup "www.google.com"
            (lambda (host family)
              (format t "Address: ~a~%" host))
            (lambda (err) (format t "err: ~a~%" err)))
{% endhighlight %}

<a id="dns-lookup-resolve-cb"></a>
##### resolve-cb definition

{% highlight cl %}
(lambda (ip-address-string ip-address-family) ...)
{% endhighlight %}

`ip-address-family` will be either `+af-inet+` or `+af-inet6+`.

<a id="start-dns-logging"></a>
### start-dns-logging
{% highlight cl %}
(defun start-dns-logging (log-cb &key event-cb))
  => nil
{% endhighlight %}

Log all DNS messages through a callback. Libevent doesn't support matching
specific messages to a DNS base, so this is a global function that will spit out
*all* DNS activity. Its main use is for debugging.

It provides an `event-cb` in case any errors occur during log processing that
your app wishes to catch.

{% highlight cl %}
;; example
(start-dns-logging
  (lambda (is-warning message)
    (format t "(~a) ~s~%" (if (zerop is-warning)
                              "log"
                              "warning")
                          message))
  :event-cb (lambda (err) (format t "err processing DNS log: ~a~%" err)))
{% endhighlight %}

<a id="start-dns-logging-log-cb"></a>
##### log-cb definition
{% highlight cl %}
(lambda (is-warning message) ...)
{% endhighlight %}

`is-warning` is an integer indicating if the message is a log or a warning, and
the `message` is a string.

<a id="stop-dns-logging"></a>
### stop-dns-logging
{% highlight cl %}
(defun stop-dns-logging ())
  => nil
{% endhighlight %}

Since logging is global and you may only want to log for a specific set of time,
you can *stop* logging DNS queries by simply calling this function.

<a id="conditions"></a>
Conditions
----------
These are the conditions the DNS system can signal in [event callbacks](/cl-async/event-handling).

<a id="dns-error"></a>
### dns-error
_extends [event-error](/cl-async/base#event-error)_

This explains a DNS error (for instance if a DNS lookup fails).

