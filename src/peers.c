
/*
 * koar/peers.c
 * copyright (c) 2013 Frano Perleta
 */

#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <errno.h>
#include <ev.h>
#include "peers.h"

// utilities {{{

static void
nonblocking (int fd)
{
    int flags = fcntl (fd, F_GETFL, 0);
    if (flags == -1)
        flags = 0;
    if (fcntl (fd, F_SETFL, flags | O_NONBLOCK) == -1)
        panic ("unable to make a file descriptor non-blocking");
}

static char*
addr_pretty (struct sockaddr_storage* ss)
{
    if (ss->ss_family == AF_UNIX)
    {
        struct sockaddr_un* sun = (void*) ss;
        size_t len = 5 + strlen (sun->sun_path);
        char* buf = xmalloc (len + 1);
        strcpy (buf, "unix:");
        strcpy (buf + 5, sun->sun_path);
        return buf;
    } else if (!(ss->ss_family == AF_INET) && !(ss->ss_family == AF_INET6))
        panic ("invalid address family %u", ss->ss_family);

    char buf[1024];
    char* p = buf; *p = 0;

    strcpy (p, (ss->ss_family == AF_INET)? "tcp:" : "tcp6:");
    p += (ss->ss_family == AF_INET)? 4 : 5;
    void* a = (ss->ss_family == AF_INET)? (void*) &(((struct sockaddr_in*) ss)->sin_addr)
                                        : (void*) &(((struct sockaddr_in6*) ss)->sin6_addr);
    in_port_t port = (ss->ss_family == AF_INET)? ((struct sockaddr_in*) ss)->sin_port
                                               : ((struct sockaddr_in6*) ss)->sin6_port;
    inet_ntop (ss->ss_family, a, p, 1023 - (p - buf));
    p += strlen (p);
    snprintf (p, 1023 - (p - buf), ":%hu", ntohs (port));

    return strdup (buf);
}

static int
addr_parse (struct sockaddr_storage* ss, socklen_t* len, const char* addr)
{
    char buf[strlen (addr) + 1];
    strcpy (buf, addr);
    const char* p = buf;

    if (!strncmp (p, "tcp:", 4))
    {
        p += 4;
        char* q = strchrnul (p, ':');
        if (*q != ':')
            goto invalid;
        *q = 0;
        q++;

        struct sockaddr_in* sin = (void*) ss;
        *len = sizeof (struct sockaddr_in);
        sin->sin_family = AF_INET;

        int ret = inet_pton (AF_INET, p, &(sin->sin_addr));
        if (ret == -1)
            panic ("inet_pton (AF_INET, ...) returned -1");
        else if (ret == 0)
            goto invalid;

        uint32_t port = 0;

        while ((*q >= '0') && (*q <= '9'))
        {
            port *= 10;
            port += *q - '0';
            if (port >= 0x10000)
                goto invalid;
            q++;
        }
        if (*q)
            goto invalid;

        sin->sin_port = htons (port & 0xFFFF);
        return 0;
    }

invalid:
    log_emit (LOG_ERROR, "invalid address: %s", addr);
    return -1;
}

// }}}

// message queues {{{

#define QSEG_MSGS (32 - 3) // to avoid memory fragmentation

typedef struct qseg_t {
    struct qseg_t* next;
    size_t start, end;
    proto_msg_t msgs[QSEG_MSGS];
} qseg_t;

typedef struct {
    qseg_t* first;
    qseg_t* last;
} queue_t;

static queue_t queue_empty = { NULL, NULL };

static void
queue_put (queue_t* q, proto_msg_t msg)
{
    qseg_t* qs;

    if (!q->first)
    {
        q-> first = q->last = qs = xmalloc (sizeof (qseg_t));
        qs->next = NULL;
        qs->start = qs->end = 0;
    }
    else if (q->last->end == QSEG_MSGS)
    {
        q->last = q->last->next = qs = xmalloc (sizeof (qseg_t));
        qs->next = NULL;
        qs->start = qs->end = 0;
    }
    else
        qs = q->last;

    qs->msgs[qs->end++] = msg;
}

static proto_msg_t
queue_get (queue_t* q)
{
    if (!q->first)
        return NULL;

    qseg_t* qs = q->first;
    proto_msg_t msg;

retry:
    if (qs->start < qs->end)
        msg = qs->msgs[qs->start++];
    else if (qs->next)
    {
        q->first = qs->next;
        free (qs);
        qs = q->first;
        goto retry;
    }
    else
    {
        qs->start = qs->end = 0;
        return NULL;
    }

    return msg;
}

// }}}

// peers {{{

typedef struct cont_t {
    struct cont_t* next;
    proto_mid_t mid;
    peer_cont_t cont;
    void* arg;
} cont_t;

struct peer_s {
    struct ev_io rd; // these two must be at fixed offsets, don't move them.
    struct ev_io wr;
    struct ev_loop* loop;
    struct sockaddr_storage sa;
    socklen_t sa_len;
    const char* name;
    peer_beh_t beh;
    cont_t* conts;
    proto_mid_t mid;
    proto_hdr_t ihead;
    proto_msg_t ibuf;
    proto_msg_t obuf;
    size_t ibytes, obytes, itotal, ototal;
    queue_t oq;
    unsigned dead;
    void* state;
};

void*
peer_get_state (peer_t peer)
{
    return peer->state;
}

void
peer_set_state (peer_t peer, void* state)
{
    peer->state = state;
}

static void
received (peer_t self, proto_msg_t msg)
{
    if (!msg->header.reply)
    {
        self->beh->on_message (self, msg);
        return;
    }

    cont_t** root = &(self->conts);
    cont_t* cont = self->conts;

    while (cont)
    {
        if (cont->mid == msg->header.mid)
        {
            *root = cont->next;
            cont->cont->on_reply (self, cont->arg, msg);
            return;
        }
        root = &(cont->next);
        cont = cont->next;
    }

    log_emit (LOG_DEBUG, "unexpected reply to message %u", msg->header.mid);
    free (msg);
}

// watchers {{{

static void
rd_w (struct ev_loop* loop, struct ev_io* io, int revents UNUSED)
{
    peer_t self = (peer_t) io;

again:
    if (!self->ibuf)
    {
        ssize_t rlen = recv
            ( io->fd
            , ((char*) &(self->ihead)) + self->ibytes
            , sizeof (proto_hdr_t) - self->ibytes
            , 0 );

        if (rlen == -1)
            return;
        else if (rlen == 0)
            goto shutdown;

        self->ibytes += (size_t) rlen;
        if (self->ibytes < sizeof (proto_hdr_t))
            return;

        // received a whole header, allocate and initialize a buffer.

        self->ihead.midr = ntohl (self->ihead.midr);
        self->ihead.bytes = ntohl (self->ihead.bytes);

        self->itotal = sizeof (proto_hdr_t) + self->ihead.bytes;
        self->ibuf = xmalloc (self->itotal);
        self->ibuf->header = self->ihead;

#if 0
        DEBUGF ("header: %smid=%lu; bytes=%lu",
                self->ihead.reply? "reply; " : "",
                self->ihead.mid, self->ihead.bytes);
#endif
    }

    size_t len = self->itotal - self->ibytes;
    ssize_t rlen = recv (io->fd, ((char*) self->ibuf) + self->ibytes, len, 0);
    if (rlen == -1)
        return;
    if (rlen == 0)
        goto shutdown;
    self->ibytes += (size_t) rlen;
    if (self->ibytes == self->itotal)
    {
        proto_msg_t msg = self->ibuf;
        self->ibuf = NULL;
        self->ibytes = 0;

        //log_emit (LOG_DEBUG, "received a message: %u %u %u", msg->header.mid, msg->header.reply, msg->header.bytes);
        received (self, msg);
        goto again;
    }
    return;

shutdown:
    DEBUGF ("peer %s shutdown", self->name);
    ev_io_stop (EV_A_ io);
    self->dead = 1;
    self->beh->on_shutdown (self);
}

static void
wr_w (struct ev_loop* loop, struct ev_io* io, int revents UNUSED)
{
    peer_t self = (peer_t) (io - 1);

again:
    if (!self->obuf)
    {
        ev_io_stop (EV_A_ io);
        return;
    }

    size_t len = self->ototal - self->obytes;
    ssize_t wlen = send (io->fd, ((char*) self->obuf) + self->obytes, len, 0);
    if (wlen == -1)
        return;
    self->obytes += (size_t) wlen;
    if (self->obytes == self->ototal)
    {
        free (self->obuf); // free the sent message
        proto_msg_t msg = self->obuf = queue_get (&(self->oq));
        self->obytes = 0;
        if (self->obuf)
        {
            self->ototal = sizeof (proto_hdr_t) + self->obuf->header.bytes;
            msg->header.midr = htonl (msg->header.midr);
            msg->header.bytes = htonl (msg->header.bytes);
        }
        goto again;
    }
}

// }}}

static void
peer_init (peer_t peer, struct ev_loop* loop, int fd, peer_beh_t beh, void* state)
{
    nonblocking (fd);

    ev_io_init (&(peer->rd), rd_w, fd, EV_READ);
    ev_io_init (&(peer->wr), wr_w, fd, EV_WRITE);

    peer->beh = beh;
    peer->conts = NULL;
    peer->mid = 42;
    peer->ibuf = peer->obuf = NULL;
    peer->ibytes = peer->obytes = 0;
    peer->oq = queue_empty;
    peer->dead = 0;
    peer->state = state;

    ev_io_start (EV_A_ &(peer->rd));
}

peer_t
peer_connect (struct ev_loop* loop, peer_beh_t beh, void* state, const char* addr)
{
    //DEBUGP ("%p", beh);
    //DEBUGP ("%s", addr);

    struct sockaddr_storage ss;
    socklen_t len;

    if (addr_parse (&ss, &len, addr) == -1)
        return NULL;

    int fd = socket (ss.ss_family, SOCK_STREAM, 0);
    if (fd == -1)
    {
        log_emit (LOG_ERROR, "socket (%d, SOCK_STREAM, 0): %s", ss.ss_family, strerror (errno));
        return NULL;
    }
    DEBUGP ("%d", fd);

    if (connect (fd, (struct sockaddr*) &ss, len) != 0)
    {
        log_emit (LOG_ERROR, "connect() to %s: %s", addr, strerror (errno));
        close (fd);
        return NULL;
    }

    nonblocking (fd);

    peer_t peer = xmalloc (sizeof (struct peer_s));
    peer->sa = ss;
    peer->sa_len = len;
    peer->name = strdup (addr);

    log_emit (LOG_DETAIL, "connected to %s", addr);
    peer_init (peer, loop, fd, beh, state);

    return peer;
}

static void
msg_write (peer_t peer, proto_msg_t msg)
{
    if (peer->obuf)
    {
        queue_put (&(peer->oq), msg);
        return;
    }

    peer->obuf = msg;
    peer->ototal = sizeof (proto_hdr_t) + msg->header.bytes;
    peer->obytes = 0;

    msg->header.midr = htonl (msg->header.midr);
    msg->header.bytes = htonl (msg->header.bytes);

    ev_io_start (EV_DEFAULT_ &(peer->wr));
}

void
peer_send (peer_t peer, proto_msg_t msg)
{
    msg->header.mid = peer->mid++;
    msg->header.reply = 0;
    msg_write (peer, msg);
}

void
peer_request (peer_t peer, proto_msg_t msg, peer_cont_t cont, void* arg)
{
    msg->header.mid = peer->mid++;
    msg->header.reply = 0;

    cont_t* c = xmalloc (sizeof (cont_t));
    c->next = peer->conts;
    c->mid = msg->header.mid;
    c->cont = cont;
    c->arg = arg;
    peer->conts = c;

    msg_write (peer, msg);
}

void
peer_reply (peer_t peer, proto_mid_t mid, proto_msg_t msg)
{
    msg->header.mid = mid;
    msg->header.reply = 1;
    msg_write (peer, msg);
}

// }}}

// listeners {{{

struct listener_s {
    struct ev_io io;
    struct ev_loop* loop;
    listener_beh_t beh;
    const char* addr;
    void* state;
};

void*
listener_get_state (listener_t listener)
{
    return listener->state;
}

void
listener_set_state (listener_t listener, void* state)
{
    listener->state = state;
}

static void
accept_w (struct ev_loop* loop, struct ev_io* io, int revents UNUSED)
{
    listener_t self = (listener_t) io;
    //DEBUGP ("%p", self);

    struct sockaddr_storage sa;
    socklen_t sa_len = sizeof (sa);
    memset (&sa, 0, sa_len);

    while (1)
    {
        int fd = accept (io->fd, (struct sockaddr*) &sa, &sa_len);
        if (fd == -1)
            return;
        //DEBUGP ("%hu", fd);

        peer_t peer = xmalloc (sizeof (struct peer_s));
        peer->sa = sa;
        peer->sa_len = sa_len;
        peer->name = addr_pretty (&sa);

        log_emit (LOG_DETAIL, "accepted %s on %s", peer->name, self->addr);
        peer_init (peer, loop, fd, &(self->beh->peer_beh), NULL);
        self->beh->on_accept (self, peer);
    }
}

listener_t
listener_create (struct ev_loop* loop, listener_beh_t beh, void* state, const char* addr)
{
    //DEBUGP ("%p", beh);
    //DEBUGP ("%s", addr);

    struct sockaddr_storage sa;
    socklen_t sa_len = sizeof (sa);
    memset (&sa, 0, sa_len);

    if (!strncmp ("tcp:", addr, 4))
    {
        struct sockaddr_in* sin = (void*) &sa;
        sin->sin_family = AF_INET;
        sin->sin_port = htons (atoi (addr + 4));
        sin->sin_addr.s_addr = INADDR_ANY;
    }
    else
    {
        log_emit (LOG_ERROR, "invalid listener address: %s:", addr);
        return NULL;
    }

    int fd = socket (sa.ss_family, SOCK_STREAM, 0);
    if (fd == -1)
    {
        log_emit (LOG_ERROR, "socket (PF_INET, SOCK_STREAM, 0): %s", strerror (errno));
        return NULL;
    }
    //DEBUGP ("%d", fd);

    nonblocking (fd);

    if (bind (fd, (struct sockaddr*) &sa, sa_len) == -1)
    {
        log_emit (LOG_ERROR, "bind (%s): %s", addr, strerror (errno));
        close (fd);
        return NULL;
    }

    if (listen (fd, 128) == -1)
    {
        log_emit (LOG_ERROR, "listen (%hu): %s", addr, strerror (errno));
        close (fd);
        return NULL;
    }

    listener_t listener = xmalloc (sizeof (struct listener_s));
    listener->loop = loop;
    listener->beh = beh;
    listener->addr = addr;
    listener->state = state;

    ev_io_init (&(listener->io), accept_w, fd, EV_READ);
    ev_io_start (EV_A_ &(listener->io));

    return listener;
}

void
listener_destroy (listener_t listener)
{
    ev_io_stop (EV_DEFAULT_ &(listener->io));
    close (listener->io.fd);
    free (listener);
}

// }}}

// vim:fdm=marker:
