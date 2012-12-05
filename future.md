---
title: Futures | Documentation
layout: default
---

Futures
=======
A future is an object that represents a value at some point later on in the
execution of an application. This is directly applicable to cl-async because
most (if not all) of the operations it performs are done asynchronously, so you
are constantly dealing with values that are not yet realized.

- [Intro to futures](#intro)
- [Integration with cl-async](#integration)
- [Futures API](#future-api)
  - [future](#future) _class_
  - [make-future](#make-future) _function_
  - [attach-errback](#attach-errback) _function_
  - [signal-error](#signal-error) _function_
  - [futurep](#futurep) _function_
  - [finish](#finish) _function_
  - [attach](#attach) _macro_
- [Nicer syntax](#nicer-syntax)
  - [alet](#alet) _macro_
  - [alet*](#alet-star) _macro_
  - [multiple-future-bind](#multiple-future-bind) _macro_
  - [wait-for](#wait-for) _macro_
- [Error handling](#error-handling)
  - [future-handler-case](#future-handler-case)


<a id="intro"></a>
Intro to futures
----------------
A future is a representation of a value in the future. The idea is that you can
attach actions to a future that will run once its value is computed, and also
attach error handlers to make sure any problems are handled along the way.

Futures not only give an important abstraction for asynchronous programming, but
offer opportunities for [syntactic abstraction](#nicer-syntax) that make async
programming a lot more natural.

Our implementation of futures are great for this because of the following
reasons:

- If a callback is [attached](#attach) to a value that is not a [future](#future),
that callback is called immediated with the value. This makes it so that you can
attach a callback to anything: a future or a value, and the end result is the
same. This way, the distiction between CPS and normal, stack-based programming
fades slightly because a function can return a future or a value, and you can
bind a callback to either.
- Calling [attach](#attach) always returns a future. This future get's fired
with the *return value of the callback being attached*. So if you have Future A
and you attach a callback to it, `attach` returns Future B. Future B gets
[finished](#finish) with the return value(s) from the callback attached to
Future A.
- [Finishing](#finish) a future with another future as the first value results
in the callbacks/errbacks from the future being finished transferring over
to the future that is passed as the value. This, in addition to [attach](#attach)
always returning a future, makes nesting futures possible. In other words, a
future can result in a future which results in a future, and if the final future
is finished with a value, the callbacks attached to the first (top-level) future
will be called with this value. This provides what's almost a call stack for
asynchronous operations in that you can derive a value from deep within a bunch
of CPS calls back to the top-level, assuming that your operations are in the
tail position (remember, a callback has to return the future from the next
operation for this to work).

This is all probably greek, so let's give an example:

{% highlight cl %}
(use-package :cl-async-future)

(defun future-calc (x)
  "Asynchronously add 1 to x, returning a future that will be finished when x is computed."
  (let ((future (make-future)))
    (as:delay (lambda () (finish future (+ x 1)))
              :time 1)
    future))

(as:start-event-loop
  (lambda ()
    (let ((future (attach (future-calc 0)
                    (lambda (x)           ;; x is 1 here
                      (attach (future-calc x)
                        (lambda (x)       ;; x is 2 here
                          (attach (future-calc x)
                            (lambda (x)   ;; x is 3 here
                              (* x 5)))))))))
      (attach future
        (lambda (x)
          (format t "Final result: ~a" x))))))
{% endhighlight %}

This waits 3 seconds then prints:

    Final result: 15

Notice how the callback was attached to the top-level future, but was able to
get the result computed from many async-levels deep. Not only does this mimick a
normal call stack a lot closer than CPS, but can be wrapped in macros that make
the syntax almost natural (note that these macros I speak of are on the way).

<a id="integration"></a>
Integration with cl-async
-------------------------
Futures are at their core, the representation of one value. Because of this, I
feel that their integration into cl-async is not appropriate. Most, if not all,
of the asynchronous operations in cl-async are stream-oriented, server oriented,
or have no values at all. The [http-client](/cl-async/http#http-client) is the
only piece that would benefit from using futures, and as such it's a bit weird
to use callbacks everywhere but one function, which uses futures.

Instead, futures are provided as a standard way to build drivers. Most drivers
provide a request-response interface, which is much more suited to futures.

So really, cl-async will at its core always use callbacks and CPS, but drivers
will be able to use futures to provide an interface that makes its users feel
less like they are programming javascript and more like programming lisp.

### Future package
The futures implementation exists under the `cl-async-future` package (`asf`)
for short. This allows an application to import `cl-async-future` into a `:use`
clause in order to gain easy access to the syntax macros, but without importing
all of cl-async as well.

<a id="future-api"></a>
Futures API
----------
<a id="future"></a>
### future (class)
The future class represents a future value. For your application, it's mostly
an opaque object which can be operated on using the functions/macros below. It
currently has no public accessors, and mainly just holds callbacks, errbacks,
values, events, etc.

The standard way to create a future is with [make-future](#make-future).

<a id="make-future"></a>
### make-future
Create a future. Supports persistent callbacks (can be fired more than once) and
reattaching callbacks to another future (when this future is [finished](#finish)
with another future as the value).

{% highlight cl %}
;; definition
(make-future &key preserve-callbacks (reattach-callbacks t))

;; example
(let ((future (make-future)))
  (attach future
    (lambda (x)
      (format t "x is ~a~%" x)))
  (finish future 5))
{% endhighlight %}

<a id="attach-errback"></a>
### attach-errback
This adds an "errback" (an error callback) to the future, to be called whenever
[signal-error](#signal-error) is called on the future. A future can hold
multiple errbacks, allowing different pieces of your application to set up
handling for different events a future might encounter.

When there are no errbacks attached to a future, any errors triggered on that
future are saved until an errback is added, at which point the errback is called
with all the saved up errors in the order they were received.

{% highlight cl %}
;; definition
(attach-errback future cb)

;; example
(let ((future (make-future))
      (socket (tcp-connect "musio.com" 80)))
  ;; set up our error/event handler
  (attach-errback future
    (lambda (ev)
      (handler-case (error ev)
        (tcp-eof () (format t "peer closed socket.~%"))
        (tcp-timeout () (format t "connection timed out.~%"))
        (t () (format t "other event: ~a~%" ev)))))

  ;; write out our request
  (write-socket-data socket (format nil "GET /~c~c" #\return #\newline)
    :read-cb (lambda (sock data) (finish future data)))

  ;; attach a cb to our heroic future
  (attach future
    (lambda (data)
      (format t "got data: ~a~%" (babel:octets-to-string data)))))
{% endhighlight %}

<a id="signal-error"></a>
### signal-error
Signal an error on the future. Many async operations will signal events/errors,
and this allows you to "transfer" these events to a future. You handle errors
on a future by [settin up errbacks](#attach-errback) on the future.

{% highlight cl %}
;; definition
(signal-error future condition)

;; example
(let ((future (make-future)))
  ;; send out a request and finish our future when we get a response, but also
  forward any events get to the future to the handler can process them
  (tcp-connect "musio.com" 80
    (lambda (sock data)
      (finish future data))
    (lambda (ev)
      ;; signal the event on the future
      (signal-error future ev))
	:data (format nil "GET /~c~c" #\return #\newline))

  ;; attach a callback to the tcp op
  (attach future
    (lambda (data)
      (format t "got data: ~a~%" (babel:octets-to-string data))))

  ;; handle any events
  (attach-errback future
    (lambda (ev)
      (format t "ev: ~a~%" ev))))
{% endhighlight %}

<a id="futurep"></a>
### futurep
Test if the given object is a future.

{% highlight cl %}
;; definition
(futurep object)
{% endhighlight %}

<a id="finish"></a>
### finish
Finish a future with one or more values. When finished, all callbacks attached
to the future will be fired, with the given values as their arguments.

{% highlight cl %}
;; definition
(finish future &rest values)

;; example
(let ((future (make-future)))
  (as:delay (lambda () (finish future 1 2 3)))
  (attach future
    (lambda (x y z)
      (format t "result: ~a~%" (* x y z)))))
{% endhighlight %}

<a id="attach"></a>
### attach
This macro attaches a callback to a future such that once the future computes,
the callback will be called with the future's finished value(s) as its
arguments.

`attach` takes two arguments, `future-gen` and `cb`. `future-gen` is a form that
*can* (but is not required to) return a future. If the first value of
`future-gen`'s return values is a future, the callback given is attached to that
future to be fired when the future's value(s) are finished.  If the first item
in `future-gen` is *not* a `future` class, the _given callback is fired
instantly with the values passed as `future-values` as the arguments_.

The reason `attach` fires the callback instantly is that it's sometimes nice to
attach a callback to a value when you don't know whether the value is a future
or an already-computed value. This allows for some useful syntactic
abstractions.

If `attach` is called on a future that has already been finished, it fires
the given callback immediately with the future's value(s).

`attach` returns one value: a future that is finished with the return values
of the given callback once it is fired. So the original future fires, the
callback gets called, and then the future that was returned from `attach` is
fired with the return values from the callback.

Also note that if a `future` is [finished](#finish) with another future as the
first value, the original future's callbacks/event handlers are _transfered_ to
the new future. This, on top of `attach` always returning a future, makes
possible some incredible syntactic abstractions which can somewhat mimick non
CPS style by allowing the results from async operations several levels deep to
be viewable by the top-level caller.

{% highlight cl %}
;; definition (future-gen is an operation that may generate multiple values)
(attach future-gen callback)

;; example
(attach (my-async-op-which-returns-a-future)
  (lambda (x)
    (format t "x is ~a~%" x)))
{% endhighlight %}

<a id="nicer-syntax"></a>
Nicer syntax
------------
Futures are a great abstraction not only because of the decoupling of an action
and a callback, but also because they can be wrapped in macros to make syntax
fairly natural. The following macros aim to be as close to native lisp as
possible while dealing with asynchronous operations.

<a id="alet"></a>
### alet
This macro allows `(let)` syntax with async functions that return futures. It
binds the future return values to the given bindings (in parallel), then runs
the body when all the futures have finished.

It's important to note that `alet` returns a future from its form, meaning it
can have a callback [attached to it](#attach), just like any other
future-generating form.

Also know that the binding forms do not not *not* have to return a future for
the binding process to work. They can return any value, and that variable will
just be bound to that value.

If an `alet` binding form results in multiple values, the first value will be 
bound to the variable (just like `let`).

{% highlight cl %}
;; definition
(alet bindings &body body)

;; example (x and y compute in parallel)
(alet ((x (grab-x-from-server))
       (y (grab-y-from-server)))
  (format t "x + y = ~a~%" (+ x y)))

;; alet can bind to nil, meaning that the future is run, but the result is
;; thrown out
(alet ((x (grab-x-from-server))
       (nil (run-command-i-dont-need-the-return-val-for)))
  (format t "got: ~a~%" x))
{% endhighlight %}

<a id="alet-star"></a>
### alet*
This macro allows `(let*)` syntax with async functions that return futures. It
binds the future return values to the given bindings (in sequence), allowing
later bindings to be able to use the values from previous bindings, and then
runs the body when all futures have calculated.

It's important to note that `alet*` returns a future from its form, meaning it
can have a callback [attached to it](#attach), just like any other
future-generating form.

Also know that the binding forms do not not *not* have to return a future for
the binding process to work. They can return any value, and that variable will
just be bound to that value.

If an `alet*` binding form results in multiple values, the first value will be 
bound to the variable (just like `let*`).

{% highlight cl %}
;; definition
(alet* bindings &body body)

;; example (note we calculate uid THEN name)
(alet* ((uid (grab-user-id-from-server))
        (name (get-user-name-from-id uid)))
  (format t "Dear, ~a. Please return my pocket lint you borrowed from me. My grandfather gave it to me and it is very important. If you do not see fit to return it, be prepared to throw down. Seriously, we're going to throw down and I'm going to straight wreck you.~%" name))

;; alet* can bind to nil, meaning that the future is run, but the result is
;; thrown out
(alet* ((x (grab-x-from-server))
        (nil (save-val x)))
  (format t "got: ~a~%" x))
{% endhighlight %}

<a id="multiple-future-bind"></a>
### multiple-future-bind
Like `multiple-value-bind` but for futures. Really, it's just a tiny macro
wrapper around [attach](#attach).

It's important to note that `multiple-future-bind` returns a future, meaning it
can have a callback [attached to it](#attach), just like any other
future-generating form.

Also note that the `future-gen` value does not have to evaluate to a future, but
any value(s), and the bindings will just attach to the given value(s) (in which
case it works exactly like `multiple-value-bind`.

{% highlight cl %}
;; definition
(multiple-future-bind ((&rest bindings) future-gen &body body))

;; example
(multiple-future-bind (id name)
    (get-user-from-server)  ; returns a future
  (format t "Hai, ~a. Your ID is ~a.~%" name id))
{% endhighlight %}

<a id="wait-for"></a>
### wait-for
Wait on a future without using any of the return values. This is good if you
want to know when an operation has finished but don't care about the result.

{% highlight cl %}
;; definition
(wait-for future-gen &body body)

;; example: run-command can return a future
(wait-for (run-command)
  (format t "Command finished.~%"))
{% endhighlight %}

<a id="error-handling"></a>
Error handling
--------------
All the wonderful [syntax macros](#nicer-syntax) in the world aren't going to do
you any good if you can't handle errors and conditions properly. The error
handling for futures closely follows how you would handle errors in native lisp.

<a id="future-handler-case"></a>
### future-handler-case
This macro wraps any of the above macros (`attach`, `alet`, `alet*`,
`multiple-future-bind`, `wait-for`) with `handler-case` like error handling.

It works not only on the top-level forms, but also on each form within the above
macros that generates a future, meaning that a single handler form can set up
error handling for all the sub-forms, even if the stack has unwound.

Note that `future-handler-case` will only work on the forms it wraps (ie the
lexical scope). If you leave via a function call or similar, it *will only catch
errors that occur in that function if they are generated synchronously*. This is
probably the main difference between `handler-case` and `future-handler-case`.
If you want to call a function that operates asynchronously from *within* a
`future-handler-case` macro, make sure that function sets up its own error
handlers.

{% highlight cl %}
;; definition
(future-handler-case body-form &rest error-forms)

;; simple example
(future-handler-case
  (alet ((record (get-record-from-server)))
    (format t "got record: ~a~%" record))
  (connection-error (e)
    (format t "oh no, a connection error.~%")))

;; nesting example

(defun process-results (x y)
  ;; any errors triggered on this stack will be caught. any errors occuring
  ;; after (calculate-z-from-server ...) returns will NOT NOT NOT be caught.
  (alet ((z (calculate-z-from-server x y)))
    (format t "z is ~a~%" z)))

(future-handler-case
  (alet ((sock (connect-to-server)))
    (future-handler-case
      (multiple-future-bind (id name)
          (get-user-from-server :socket sock)
        (alet* ((x (get-x-from-server :socket sock))
                (y (get-y-from-server :socket sock)))
          (format t "x+y: ~a~%" (+ x y))
          (process-results x y)))
      (type-error (e)
        (format t "Got a type error, x or y possibly isn't a number: ~a~%" e))))
  (connection-error (e)
    (format t "Error connecting to server: ~a~%" e))
  (t (e)
    (format t "Got general error: ~a~%" e)))
{% endhighlight %}

In the above, if `x` or `y` are not returned as numbers, it will be caught by
the `(type-error ...)` handler. If some unknown error occurs anywhere inside the
top-level `future-handler-case`, the outer `(t (e) ...)` general error handler
will get triggered (even though there's a `future-handler-case` inside it).

If `process-results` signals an error, it will only be caught by the
`future-handler-case` forms if it spawned no asynchronous events _or_ if any
errors signaled are done so on the current stack (ie synchronously, *not*
asynchronously).

Really, if you want to call out to another function that performs asynchronous
operations from within a `future-handler-case`, make sure that function is
perfectly capable of handling its own errors without relying on the calling form
to catch them *OR ELSE*.
