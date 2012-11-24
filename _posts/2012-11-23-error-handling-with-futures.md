---
title: Error handling with futures
layout: post
---
So in the [last installment of futures updates](/cl-async/2012/11/15/adding-syntax-around-futures),
we added nice syntax around futures that makes working with them *close* to
native lisp.

The horrible problem is that we forgot to touch on error handling. This is not
because I had forgotten about it, but because I had not actually programmed them
with error handling in mind. That all changes today!

I made a macro, [future-handler-case](/cl-async/future#future-handler-case),
that adds near-native (as usual) error handling for future-generating forms. It
sits nicely on top of our friends [attach](/cl-async/future#attach), [alet](/cl-async/future#alet),
[alet*](/cl-async/future#alet-star), [multiple-future-bind](/cl-async/future#multiple-future-bind),
and [wait-for](/cl-async/future#wait-for) and allows you to easily wrap their
future-generating forms with one top-level handler.

{% highlight cl %}
(future-handler-case
  (multiple-future-bind (name age)
      (get-user-from-server)
    (format t "got user ~a, who is ~a years old.~%" name age))
  (user-not-found ()
    (format t "Sorry, user wasn't found.~%"))
  (connection-error ()
    (format t "Error connecting to server.~%)))
{% endhighlight %}

So the syntax is like that of `handler-case`. The power of it comes from the
fact that even after the stack unwinds, the operations it wraps will still have
the error handlers assigned by `future-handler-case`.

The one caveat is that if you jump out of `future-handler-case` into a function
that has asynchronous operations, it will not be able to catch errors triggered
asynchronously in that function (because it needs access to the markup):

{% highlight cl %}
(defun process-results (x y)
  (alet ((z (get-z-from-server x y)))
    (unless (= z 5)
      ;; this will not be caught, and will be sent tot he REPL
      (error "Z is incorrect!"))))

(future-handler-bind
  (alet* ((x (get-x-from-server))
          (y (get-y-from-server)))
    (format t "Got x, y: ~a ~a~%" x y)
    (process-results x y))
  (t (e)
    (format t "Got error: ~a~%" e)))
{% endhighlight %}

So in the example if x + y != 5, an error will be triggered, but will *not* be
caught by the error handler because it happens after `process-results` has
returned. What you want to take away from this is that if you are calling out to
a function that performs asynchronous operations, it needs to have its own
`future-handler-case` wrapping it to be able to *safely* process any errors that
might occur inside of it.

Aside from that, `future-handler-case` closely models `handler-case` except that
it exists asynchronously for all future-generating forms it wraps. This makes
error handling when dealing with futures feel more like native lisp.
