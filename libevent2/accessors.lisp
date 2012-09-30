(in-package :libevent2.accessors)

(defmacro make-accessors (c-struct)
  `(progn
     ,@(loop for slot-name in (foreign-slot-names (intern (string c-struct) :libevent2))
             for accessor-name = (intern (concatenate 'string (symbol-name c-struct)
                                                      "-"
                                                      (symbol-name slot-name)))
             append (list `(defmacro ,accessor-name (ptr)
                             (list 'foreign-slot-value ptr '',(intern (string c-struct) :libevent2) '',slot-name))
                          `(export ',accessor-name :libevent2.accessors)))))

(make-accessors #.(libevent2::lispify "timeval" 'classname))
(make-accessors #.(libevent2::lispify "sockaddr_in" 'classname))
(make-accessors #.(libevent2::lispify "evkeyval" 'classname))
(make-accessors #.(libevent2::lispify "evkeyvalq" 'classname))
(make-accessors #.(libevent2::lispify "evbuffer_ptr" 'classname))
(make-accessors #.(libevent2::lispify "evbuffer_ptr__internal" 'classname))
(make-accessors #.(libevent2::lispify "evbuffer_iovec" 'classname))
(make-accessors #.(libevent2::lispify "evbuffer_cb_info" 'classname))
