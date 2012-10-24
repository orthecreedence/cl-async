---
title: Home
layout: default
---

test page

{% highlight cl %}
(defun lol ()
  (format t "Testing.~%"))
(as:delay (lambda () (lol)) :time 6)
{% endhighlight %}

end highlight
