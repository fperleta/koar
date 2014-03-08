
/*
 * koar/patchctl.c
 * copyright (c) 2013 Frano Perleta
 */

#include <string.h>
#include <ev.h>
#include <pthread.h>
#include "patchvm.h"
#include "patchctl.h"

// patchctls {{{

struct patchctl_s {
    patchctl_endpoint_t endpoint;
    size_t index, workers;
    patchctl_state_t state;

    patchctl_state_t errstate;
    const char* errname;
    char* errtext;

    patchvm_t vm;
};

static size_t ep_up (patchctl_endpoint_t, patchctl_t);
static void ep_down (patchctl_endpoint_t, patchctl_t);

static patchctl_t
patchctl_create (patchctl_endpoint_t ep, size_t index)
{
    patchctl_t ctl = xmalloc (sizeof (struct patchctl_s));

    ctl->workers = ep_up (ep, ctl);

    ctl->endpoint = ep;
    ctl->index = index;

    ctl->state = ctl->errstate = PATCHCTL_FREE;
    ctl->errname = ctl->errtext = NULL;

    ctl->vm = NULL;

    return ctl;
}

static void
patchctl_destroy (patchctl_t ctl, int down)
{
    if (down)
        ep_down (ctl->endpoint, ctl);

    if (ctl->errtext)
        free (ctl->errtext);

    if (ctl->vm)
        patchvm_destroy (ctl->vm);

    free (ctl);
}

// }}}

// the protocol {{{

// utilities {{{

MACRO void
short_reply (peer_t self, proto_mid_t mid, const char* str)
{
    size_t len = strlen (str);
    proto_msg_t msg = proto_msg_alloc (len);
    memcpy (msg->payload, str, len);
    peer_reply (self, mid, msg);
}

MACRO int
match_msg (proto_msg_t msg, const char* str)
{
    size_t len = strlen (str);
    return (len == msg->header.bytes) && !memcmp (str, msg->payload, len);
}

// }}}

// state FREE {{{

static void
free_enter (patchctl_t ctl)
{
    if (ctl->vm)
    {
        patchvm_destroy (ctl->vm);
        ctl->vm = NULL;
    }
    ctl->state = PATCHCTL_FREE;
}

static void
free_on_patch (peer_t self, proto_msg_t msg)
{
    patchctl_t ctl = peer_get_state (self);

    proto_mid_t mid = msg->header.mid;
    free (msg);

    ctl->state = PATCHCTL_BOUND;
    ctl->vm = patchvm_create (ctl->workers, 0);

    short_reply (self, mid, "okay");
}

// }}}

static void error_enter (peer_t, const char*, const char*);

// state BOUND {{{

static void
bound_on_message (peer_t self, proto_msg_t msg)
{
    patchctl_t ctl = peer_get_state (self);
    patchvm_exec (ctl->vm, msg->payload, msg->header.bytes);
    proto_mid_t mid = msg->header.mid;
    free (msg);

    if (patchvm_failed (ctl->vm))
        error_enter (self, "vm", "Virtual machine error.");
    else if (patchvm_leaving (ctl->vm))
    {
        short_reply (self, mid, "free");
        free_enter (ctl);
    }
    else
        short_reply (self, mid, "okay");
}

// }}}

// state ERROR {{{

static void
error_enter (peer_t self, const char* errname, const char* errtext)
{
    patchctl_t ctl = peer_get_state (self);

    ctl->errstate = ctl->state;
    ctl->state = PATCHCTL_ERROR;
    ctl->errname = errname;
    ctl->errtext = strdup (errtext);
}

static void
error_on_message (peer_t self UNUSED, proto_msg_t msg)
{
    free (msg);
}

// }}}

static void
patchctl_on_message (peer_t self, proto_msg_t msg)
{
    patchctl_t ctl = peer_get_state (self);

    log_emit (LOG_DETAIL, "patchctl %p received a message in state %d.", ctl, ctl->state);

    switch (ctl->state)
    {
        case PATCHCTL_FREE:
            if (match_msg (msg, "patch"))
                free_on_patch (self, msg);
            else
            {
                error_enter (self, "wat", "An incomprehensible message was received from the client.");
                free (msg);
            }
            break;

        case PATCHCTL_BOUND:
            bound_on_message (self, msg);
            break;

        case PATCHCTL_ERROR:
            error_on_message (self, msg);
            break;

        default:
            free (msg);
            break;
    }
}

static void
patchctl_on_shutdown (peer_t self)
{
    patchctl_t ctl = peer_get_state (self);
    patchctl_destroy (ctl, 1);
}

// }}}

// endpoints {{{

struct patchctl_endpoint_s {
    pthread_mutex_t mutex;
    listener_t listener;
    struct ev_loop* loop;
    size_t used, limit;
    size_t workers;
    patchctl_t *ctls;
    size_t next;
};

static size_t
ep_up (patchctl_endpoint_t ep, patchctl_t ctl)
{
    pthread_mutex_lock (&(ep->mutex));

    if (ep->used == ep->limit)
        panic ("too many patchctls");

    ep->ctls[ep->used++] = ctl;
    size_t workers = ep->workers;

    pthread_mutex_unlock (&(ep->mutex));

    return workers;
}

static void
ep_down (patchctl_endpoint_t ep, patchctl_t ctl)
{
    pthread_mutex_lock (&(ep->mutex));

    size_t i;
    for (i = 0; (i < ep->used) && (ep->ctls[i] != ctl); i++);
    if (i < ep->used)
    {
        for (; i + 1 < ep->used; ep->ctls[i] = ep->ctls[i+1], i++);
        ep->ctls[--ep->used] = NULL;
    }

    pthread_mutex_unlock (&(ep->mutex));
}

static void
endpoint_on_accept (listener_t self, peer_t peer)
{
    patchctl_endpoint_t ep = listener_get_state (self);
    log_emit (LOG_DETAIL, "endpoint %p accepting...", ep);

    pthread_mutex_lock (&(ep->mutex));
    size_t index = ep->next++;
    pthread_mutex_unlock (&(ep->mutex));

    patchctl_t ctl = patchctl_create (ep, index);
    peer_set_state (peer, ctl);

    log_emit (LOG_DETAIL, "endpoint %p created a new patchctl %p", ep, ctl);
}

static struct listener_beh_s
endpoint_beh = {
    .on_accept = endpoint_on_accept,
    .peer_beh = {
        .on_message = patchctl_on_message,
        .on_shutdown = patchctl_on_shutdown
    }
};

patchctl_endpoint_t
patchctl_endpoint_create (struct ev_loop* loop, const char* addr, size_t limit, size_t workers)
{
    DEBUGP ("%s", addr);

    listener_t listener = listener_create (loop, &endpoint_beh, NULL, addr);
    if (!listener)
        return NULL;

    patchctl_endpoint_t ep = malloc (sizeof (struct patchctl_endpoint_s));
    listener_set_state (listener, ep);

    int res = pthread_mutex_init (&(ep->mutex), NULL);
    if (res)
        panic ("pthread_mutex_init() returned %d", res);

    ep->loop = loop;
    ep->listener = listener;
    ep->used = 0;
    ep->limit = limit;
    ep->workers = workers;
    ep->ctls = xmalloc (sizeof (patchctl_t) * limit);
    ep->next = 1;

    log_emit (LOG_DETAIL, "endpoint %p created at address %s", ep, addr);

    return ep;
}

void
patchctl_endpoint_destroy (patchctl_endpoint_t ep)
{
    pthread_mutex_lock (&(ep->mutex));

    listener_destroy (ep->listener);

    size_t i;
    for (i = 0; i < ep->used; i++)
        patchctl_destroy (ep->ctls[i], 0);
    free (ep->ctls);

    pthread_mutex_unlock (&(ep->mutex));

    int res = pthread_mutex_destroy (&(ep->mutex));
    if (res)
        panic ("pthread_mutex_destroy() returned %d", res);

    free (ep);

    log_emit (LOG_DETAIL, "endpoint %p destroyed", ep);
}

// }}}

// vim:fdm=marker:
