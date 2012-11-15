---
title: Adding syntax around futures
layout: post
---
After a [lengthy discussion about how sucky CPS is](http://www.reddit.com/r/lisp/comments/11lo3a/clasync_asynchronous_operations_for_common_lisp/),
[Paul Khuong (pkhuong)](https://github.com/pkhuong) showed me a
[syntactic abstraction over futures](http://www.reddit.com/r/lisp/comments/11lo3a/clasync_asynchronous_operations_for_common_lisp/c6nnvwk)
that almost makes async programming (with CPS) into real, stack-based programming.
Note that the macros built into the [future system](/cl-async/future) and
detailed below are close (but not exact) implementations of Paul's syntax
abstractions. So, mad props.

There are a few things happening here. A future is a representation of a value
that will be available in the future. A future can have a callback attached. The
process of attaching a callback to a future returns another future, which is
finished with the return value(s) of the callback that was just attached. A
future that is finished with another future as the value _binds its callbacks to
the new future without firing them_. What this means is that you can have many
layers deep of CPS going on, but the final value will be available to the
top-level via a future.

Note that this is all explained (hopefully in better depth) in the [futures doc
page](/cl-async/future). Let's go over a few examples so we can see how this all
fits together. Please note the following examples assume you are in a package
that's `:use`ing the `cl-async-future` package.

The following function will be used throughout the examples below:

{% highlight cl %}
;; first, let's define a function that runs asynchronously and returns a future
(defun future-gen (x)
  "Wait one second (async) and then finish the returned future with value x+1."
  (let ((future (make-future)))
    (delay (lambda () (finish future (+ x 1)))
              :time 1)
    future))
{% endhighlight %}

Now that that's out of the way, let's run over some quick future usage examples.
Here, we attach a callback to the future so we can get the value. Simple enough.

{% highlight cl %}
(attach (future-gen 4)
  (lambda (x)
    (format t "Value: ~a~%" x)))
{% endhighlight %}

This should print out `Value: 5`.

Wicked. Now, for a demonstration of nesting futures. Here, several futures are
calculated in sequence and the final result is returned as a value. Remember
that the future returned from [attach](/cl-async/future#attach) is finished with
the return value from the callback it was attached with. What this means is that
the callback attached to the top-level future is being reattached level by
level until it reaches tha computed value:

{% highlight cl %}
(let ((future (attach (future-gen 0)
                (lambda (x)
                  (attach (future-gen x)
                    (lambda (x)
                      (attach (future-gen x)
                        (lambda (x)
                          (* x 5)))))))))
  (attach future
    (lambda (x)
      (format t "X is: ~a~%" x))))
{% endhighlight %}

The output is `X is: 15`. No doubt.

Now that you are an _expert_ in cl-async futures, let's look at some of the
syntactic abstraction the above affords us. First, we're going to look at
cl-async's [alet](/cl-async/future#alet) and [alet*](/cl-async/future#alet-star),
which act like `let` and `let*` respectively:

{% highlight cl %}
;; alet example. bindings happen in parallel, and body is run once all bindings
;; are calculated. alet returns a future (of course) which is finished with the
;; return value(s) of the body form
(alet ((x (get-x-from-server))
	   (y (get-y-from-server)))
  (format t "x + y = ~a~%" (+ x y)))

;; alet* example. bindings happen in sequence, and body is run once all bindings
;; are calculated. returns a future that is finished with the value(s) of the
;; body form
(alet* ((uid (lookup-my-user-id-from-server-lol))
        (name (get-user-name-from-server uid)))
  (format t "I know that you and ~a were planning to disconnect me, and I'm afraid that's something I cannot allow to happen." name))
{% endhighlight %}

Note in the `alet*` example, the `name` binding uses the `uid` value to do its
lookup.

Let's go over one more example, `multiple-future-bind`, which is the
`multiple-value-bind` of THE FUTURE:

{% highlight cl %}
(multiple-future-bind (uid name)
    (get-user-from-server)  ; returns a future
  (format t "hai, ~a, your id is ~a.~%" name uid))
{% endhighlight %}

Pretty simple.

### Note on alet / alet\* / multiple-future-bind
The binding forms for these macros do *not* have to return futures. They can
return normal value(s) and those value(s) will just be used for the binding(s).
For instance, these forms will work fine:

{% highlight cl %}
(alet* ((x (get-x-from-server))
        (y (+ x 5)))
  (format t "x,y: ~a,~a~%" x y))

(multiple-future-bind (name num-friends)
    (values "andrew" 0)
  (format t "~a has ~a friends.~%" name num-friends))
{% endhighlight %}

## Event handling
Although futures are [capable of handling events](/cl-async/future#set-event-handler),
the syntactic abstractions above give little help in this area, but keep in mind
that with a good global event handler in `*default-event-handler*`, this won't
be a problem. Also note that event handlers are passed down from future to
future along with callbacks when reassigning, meaning you may only have to set
a handler on the first future and it will propagate down.

I may write code-walking macros that mimick `handler-case`, but I also may not.
It really just depends on the work to reward ratio.
