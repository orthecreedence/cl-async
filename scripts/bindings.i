%module bindings

%feature("intern_function", "lispify");

%insert("lisphead") %{
(in-package :libevent2)
%}

%ignore "_EVENT_LT_OBJDIR";
%ignore "_EVENT_VERSION";

#define AF_UNSPEC   0
#define AF_UNIX 1
#define AF_INET 2

typedef unsigned int size_t;

struct timeval {
  long tv_sec;
  long tv_usec;
};

struct sockaddr_in {
    short   sin_family;
    unsigned short sin_port;
    unsigned long sin_addr;
    /*struct  in_addr sin_addr;*/
    /*char    sin_zero[8];*/
    char sin_zero_0;
    char sin_zero_1;
    char sin_zero_2;
    char sin_zero_3;
    char sin_zero_4;
    char sin_zero_5;
    char sin_zero_6;
    char sin_zero_7;
};

%include "/usr/local/libevent/include/event2/event-config.h"
%include "/usr/local/libevent/include/event2/util.h"
%include "/usr/local/libevent/include/event2/event.h"
%include "/usr/local/libevent/include/event2/dns.h"
%include "/usr/local/libevent/include/event2/bufferevent.h"
%include "/usr/local/libevent/include/event2/buffer.h"
%include "/usr/local/libevent/include/event2/listener.h"
