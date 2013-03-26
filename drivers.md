---
title: Drivers built on cl-async
layout: default
---

Drivers built on cl-async
=========================

- [drakma-async](https://github.com/orthecreedence/drakma-async)  
- [beanstalkd](https://github.com/orthecreedence/beanstalk-async)  
- [MongoDB](https://github.com/archimag/mongo-cl-driver)  
  mongo-cl-driver allows different connection backends to MongoDB, one of them
  being a cl-async interface.
- [Wookie](https://github.com/orthecreedence/wookie)  
  Originally started as a Hunchentoot port, Wookie is now its own async HTTP
  web server.
- [green-threads](https://github.com/deliciousrobots/green-threads)  
  This excellent project builds green threads on top of cl-cont, which can
  eliminate CPS-style in async programming by building on top of cl-async's
  futures implementation.

The list grows! Thanks to everyone who has contributed projects/time/ideas.

Planned
=======
- Amazon S3/Cloudfront
- SMTP
- Redis
- Postgres
- RethinkDB

