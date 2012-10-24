---
title: Drivers built on cl-async
layout: default
---

Drivers built on cl-async
=========================
A number of drivers are planned to be built on top of cl-async

- [beanstalkd](https://github.com/orthecreedence/beanstalk-async)
- MongoDB
- Drakma (async port, would essentially replace [http-client](/cl-async/http#http-client))
- Amazon S3/Cloudfront
- SMTP
- Redis

Note that these are libraries I use every day, so am in a good position to test
them in a production environment. Also, even though cl-async includes a simple
HTTP client, [Drakma](http://weitz.de/drakma/) is a lot more badass and has a
ton more features. Porting it to be asynchronous would be very valuable, and
also would make porting other drivers that work over HTTP to cl-async easier.

The biggest problem with asynchronous IO in lisp is that there are lots of
libraries that provide it, but no drivers built on top of the libraries. Nobody
wants to sit around all day programming database drivers. I think if I get
enough traction behind cl-async by providing drivers for enough services, it
could stand to be the first viable asynchronous programming library for Common
Lisp users.

So all I need is critical mass. WHO'S WITH ME?!?!

