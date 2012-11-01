---
title: First working version of drakma-async client finished
layout: post
---
Since my [previous HTTP client update](/cl-async/2012/10/31/http-client-update.html)
I've made a good amount of progress. I created an HTTP parser entirely in lisp
that is able to determine the readiness of an HTTP response. It supports
content-length and chunking response types. It does not currently check for
`100 Continue` responses yet, but once it does, I think it will be complete.

So the general workflow is as such:

 1. Init http-client-stream, send request into returned stream. If doing this
 through [drakma-async](https://github.com/orthecreedence/drakma-async),
 a [future](/cl-async/2012/10/25/playing-with-futures.html) is returned, which
 will be triggered when the request completes.
 2. Wait for data on the socket. Once data comes in from the server, we remove
 it from the socket and pass it into the HTTP parser, which stores it internally
 and checks if the response is complete.
 3. If the response is not complete, we wait for the next round of data to come
 in. If the response is complete, we take the data stored in the HTTP parser and
 shove it back into the beginning of the original socket stream. We then pass
 that stream (containing all the data from the complete response) into
 drakma-async, which parses the stream just like drakma does normally.
 4. The future drakma-async returned is finished with the values normally passed
 back from drakma, and the calling application can attach a callback to get the
 results.

From my initial testing, this is working wonderfully. Aside from any unknown
bugs in the HTTP parser, the only case not being handled is `100 Continue` being
passed back from the server. Otherwise, [drakma-async](https://github.com/orthecreedence/drakma-async)
is now in beta.

