%module bindings

%feature("intern_function", "lispify");

%insert("lisphead") %{
(in-package :libevent2)
%}

%ignore "_EVENT_LT_OBJDIR";
%ignore "_EVENT_VERSION";
%ignore "_EVENT_PACKAGE";
%ignore "_EVENT_PACKAGE_BUGREPORT";
%ignore "_EVENT_PACKAGE_NAME";
%ignore "_EVENT_PACKAGE_STRING";
%ignore "_EVENT_PACKAGE_TARNAME";
%ignore "_EVENT_PACKAGE_URL";
%ignore "_EVENT_PACKAGE_VERSION";
%ignore "LIBEVENT_VERSION";

#define AF_UNSPEC   0
#define AF_UNIX 1
#define AF_INET 2
#define AF_INET6 23
#define SOCK_STREAM 1
#define IPPROTO_TCP 6

typedef unsigned long size_t;

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

struct sockaddr_in6 {
    unsigned short sin6_family;
    unsigned short sin6_port;
    unsigned int sin6_flowinfo;
    /* struct sin6_addr */
    /* unsigned char s6_addr[16] */
    unsigned char sin6_addr_0;
    unsigned char sin6_addr_1;
    unsigned char sin6_addr_2;
    unsigned char sin6_addr_3;
    unsigned char sin6_addr_4;
    unsigned char sin6_addr_5;
    unsigned char sin6_addr_6;
    unsigned char sin6_addr_7;
    unsigned char sin6_addr_8;
    unsigned char sin6_addr_9;
    unsigned char sin6_addr_10;
    unsigned char sin6_addr_11;
    unsigned char sin6_addr_12;
    unsigned char sin6_addr_13;
    unsigned char sin6_addr_14;
    unsigned char sin6_addr_15;
    unsigned int sin6_scope_id;
};

struct addrinfo {
    int     ai_flags;
    int     ai_family;
    int     ai_socktype;
    int     ai_protocol;
    size_t  ai_addrlen;
    struct sockaddr  *ai_addr;
    char   *ai_canonname;
    struct evutil_addrinfo  *ai_next;
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

%include "/usr/local/include/event2/event-config.h"
%include "/usr/local/include/event2/util.h"

typedef unsigned short ev_uint16_t;

%include "/usr/local/include/event2/event.h"
%include "/usr/local/include/event2/dns.h"
%include "/usr/local/include/event2/bufferevent.h"
%include "/usr/local/include/event2/bufferevent_struct.h"
%include "/usr/local/include/event2/buffer.h"
%include "/usr/local/include/event2/listener.h"
%include "/usr/local/include/event2/http.h"

