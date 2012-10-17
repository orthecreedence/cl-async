Libevent2 bindings for Common Lisp
==================================
Please note that these bindings aren't fully documented, and unless someone
actually wants to take on this task, they will most likely stay that way as
they were built/generated as a means of driving [cl-async](https://github.com/orthecreedence/cl-async).

Conventions
-----------
Who needs documentation when you follow simple function-naming conventions?

- The package prefix is `le:`
- Underscores become dashes

That's actually it.

### Example
```c
struct event *ev;
struct event_base *base = event_base_new();
ev = event_new(base, -1, 0, my_cb, 0);
event_active(ev, 0, 0);
event_base_dispatch(base);
event_del(ev);
```

Becomes:

```common-lisp
(let* ((base (le:event-base-new))
       (ev (le:event-new base -1 0 (cffi:callback my-cb) (cffi:null-pointer))))
  (le:event-active ev 0 0)
  (le:event-base-dispatch base)
  (le:event-del(ev)))
```

(re)Generating
--------------
If a new version of libevent comes out, you can regenerate these bindings by
doing the following (if you have [swig](http://www.swig.org/) installed):

```bash
cd /path/to/asdf/libevent2
vim scripts/bindings.i      # update "%include" paths to point at your libevent headers
./scripts/generate          # must be run in libevent2 folder
```

This will generate new bindings in their entirety (it's fully automated).

Notes
-----
As mentioned, these bindings were made specifically to be the backend for
[cl-async](https://github.com/orthecreedence/cl-async), and because of this,
they do not (nor will they ever) have a higher-level interface. They are meant
to be an extremely thin layer between Lisp and c/libevent.

MIT Licensed.
