---
title: Home
layout: default
---

Asynchronous operations for Common Lisp
=======================================
Cl-async is a library for general purpose, non-blocking programming in Common
Lisp.

The main goal is to provide an experience that makes general asynchronous 
programming in lisp natural, and to also provide a number of
[drivers](/cl-async/drivers) on top of cl-async.

*This library's current status is BETA.* Please see the
[Issues list](https://github.com/orthecreedence/cl-async/issues) for the
complete list of what needs to be done.

<div class="callout">
	<ul class="clear">
		<li><a href="/cl-async/documentation">Docs</a></li>
		<li><a href="/cl-async/examples">Examples</a></li>
		<li><a href="https://github.com/orthecreedence/cl-async">Github</a></li>
	</ul>
</div>

### [Windows binaries](/cl-async/files/libevent-2.0.20-x86-bin.zip)
People have had trouble getting the libevent library compiled on Windows, so
I've decided to make some window binaries:

- [Windows 32bit DLL package](/cl-async/files/libevent-2.0.20-x86-bin.zip)
- [Windows 64bit DLL package](/cl-async/files/libevent-2.0.20-x64-bin/zip)

Unzip anywhere in your %PATH% and you should be able to jump right in without
problems. Note that the 32bit library is recommended (and contains the
`libevent-openssl` DLL missing in 64bit) but in some cases 64bit is needed
(like when running CCL64 which cannot load 32bit DLLs).

## Library updates
{% for post in site.posts %}
##### [{{ post.title }}](/cl-async{{ post.url }}) - <small>{{ post.date | date: "%b %d, %Y" }}</small>
{% endfor %}
