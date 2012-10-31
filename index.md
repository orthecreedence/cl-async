---
title: Home
layout: default
---

Asynchronous operations for Common Lisp
=======================================
Cl-async is a library for general purpose library for non-blocking programming
Common Lisp.

The main goal is to provide an experience that makes general asynchronous 
programming in lisp a delight instead of a chore. Portability and ease of use
are favored over raw speed.

*This library's current status is BETA.* Please see the
[Issues list](https://github.com/orthecreedence/cl-async/issues) for the
complete list of what needs to be done.

<div class="callout">
	<ul class="clear">
		<li><a href="/documentation">Docs</a></li>
		<li><a href="/examples">Examples</a></li>
		<li><a href="https://github.com/orthecreedence/cl-async">Github</a></li>
	</ul>
</div>

## News
{% for post in site.posts %}
### [{{ post.title }}](/cl-async{{ post.url }}) <small>{{ post.date | date_to_long_string }}</small>
{% endfor %}
