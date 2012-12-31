---
title: Tests
layout: default
---

Tests
=====
Cl-async comes with a test suite under the `cl-async-test` package. Tests ca be
loaded and run via:

{% highlight cl %}
(ql:quickload :cl-async-test)
(cl-async-test:run-tests)
{% endhighlight %}

This will run all the tests on the core cl-async library.

SSL
---
Tests also exist for [cl-async's SSL implementation](/cl-async/tcp-ssl) and can
be loaded/run simply by telling `run-tests` to do so:

{% highlight cl %}
(cl-async-test:run-tests :ssl t)
{% endhighlight %}

Notes
-----
If you get errors running the test suite (or the tests don't pass), please be
sure to [open a github issue](https://github.com/orthecreedence/cl-async/issues)
so it can be fixed for your implementation/OS.
