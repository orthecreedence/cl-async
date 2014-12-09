---
title: DNS | Documentation
layout: documentation
---

DNS
===
This section details resolving hostnames to addresses, as well as
conditions/events that occur while using the DNS system.

- [dns-lookup](#dns-lookup) _function_
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

<a id="conditions"></a>
Conditions
----------
These are the conditions the DNS system can signal in [event callbacks](/cl-async/event-handling).

<a id="dns-error"></a>
### dns-error
_extends [event-error](/cl-async/base#event-error)_

This explains a DNS error (for instance if a DNS lookup fails).

