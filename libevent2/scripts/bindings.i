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
#define AF_INET6 23
#define SOCK_STREAM 1
#define IPPROTO_TCP 6

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

struct evkeyval {
    struct evkeyval *next;
    struct evkeyval **prev;

    char* key;
    char* value;
};

struct evkeyvalq {
    struct evkeyval *thq_first;
    struct evkeyval **thq_last;
};

struct evutil_addrinfo {
    int     ai_flags;     /* AI_PASSIVE, AI_CANONNAME, AI_NUMERICHOST */
    int     ai_family;    /* PF_xxx */
    int     ai_socktype;  /* SOCK_xxx */
    int     ai_protocol;  /* 0 or IPPROTO_xxx for IPv4 and IPv6 */
    size_t  ai_addrlen;   /* length of ai_addr */
    char   *ai_canonname; /* canonical name for nodename */
    struct sockaddr  *ai_addr; /* binary address */
    struct evutil_addrinfo  *ai_next; /* next structure in linked list */
};

%include "/usr/local/libevent/include/event2/event-config.h"
%include "/usr/local/libevent/include/event2/util.h"

typedef unsigned int ev_uint16_t;

%include "/usr/local/libevent/include/event2/event.h"
%include "/usr/local/libevent/include/event2/dns.h"
%include "/usr/local/libevent/include/event2/bufferevent.h"
%include "/usr/local/libevent/include/event2/bufferevent_struct.h"
%include "/usr/local/libevent/include/event2/buffer.h"
%include "/usr/local/libevent/include/event2/listener.h"
%include "/usr/local/libevent/include/event2/http.h"

