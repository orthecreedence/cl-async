---
title: Renaming base conditions (breaking change)
layout: post
---
This is a quick update: I have performed the following rename of the base
conditions:

{% highlight cl %}
connection-info   ->  event-info
connection-error  ->  event-error
conn-errcode      ->  event-errcode
conn-errmsg       ->  event-errmsg
{% endhighlight %}

This breaks the API. Sorry. Deprecated version not available.

Having `connection-*` everything is an old throwback to the time when cl-async
only dealt with async TCP/HTTP (so, it's very first few days on this planet). It
made sense to have base event conditions back then, but I didn't do it, and I've
had to live with that decision all this time. Until now.

Normally I'll deprecate something (the case with many of the API functions I've
changed over the last few months) and export both the old and the new, but in
this case I feel this change is important enough to force people to upgrade. I
could just be OCD, and I could also realize the fact that probably very few
people are using this library anyway, so a breaking API change only breaks it
for me ='[.

