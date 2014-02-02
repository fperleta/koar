
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
    size_t index;
    patchctl_state_t state;

    patchctl_state_t errstate;
    const char* errname;
    char* errtext;

    patchvm_t vm;
};

static void ep_up (patchctl_endpoint_t, patchctl_t);
static void ep_down (patchctl_endpoint_t, patchctl_t);

static patchctl_t
patchctl_create (patchctl_endpoint_t ep, size_t index)
{
    patchctl_t ctl = xmalloc (sizeof (struct patchctl_s));

    ep_up (ep, ctl);

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
free_on_patch (patchctl_t ctl, proto_msg_t msg)
{
    ctl->vm = patchvm_create (0);
    free (msg);
}

// }}}

static void error_enter (peer_t, const char*, const char*);

// state BOUND {{{

static void
bound_on_message (peer_t self, proto_msg_t msg)
{
    patchctl_t ctl = peer_get_state (self);
    patchvm_exec (ctl->vm, msg->payload, msg->header.bytes);
    free (msg);

    if (patchvm_failed (ctl->vm))
        error_enter (self, "vm", "Virtual machine error.");
    if (patchvm_leaving (ctl->vm))
        free_enter (ctl);
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

    switch (ctl->state)
    {
        case PATCHCTL_FREE:
            if (match_msg (msg, "patch"))
                free_on_patch (ctl, msg);
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
patchctl_on_shutdown (peer_t self UNUSED)
{
}

// }}}

// endpoints {{{

struct patchctl_endpoint_s {
    pthread_mutex_t mutex;
    listener_t listener;
    struct ev_loop* loop;
    size_t used, limit;
    patchctl_t *ctls;
    size_t next;
};

static void
ep_up (patchctl_endpoint_t ep, patchctl_t ctl)
{
    pthread_mutex_lock (&(ep->mutex));

    if (ep->used == ep->limit)
        panic ("too many patchctls");

    ep->ctls[ep->used++] = ctl;

    pthread_mutex_unlock (&(ep->mutex));
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

    pthread_mutex_lock (&(ep->mutex));

    patchctl_t ctl = patchctl_create (ep, ep->next++);
    peer_set_state (peer, ctl);

    pthread_mutex_unlock (&(ep->mutex));
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
patchctl_endpoint_create (struct ev_loop* loop, const char* addr, size_t limit)
{
    DEBUGP ("%s", addr);

    patchctl_endpoint_t ep = malloc (sizeof (struct patchctl_endpoint_s));

    int res = pthread_mutex_init (&(ep->mutex), NULL);
    if (res)
        panic ("pthread_mutex_init() returned %d", res);

    ep->loop = loop;
    ep->listener = listener_create (loop, &endpoint_beh, ep, addr);
    ep->used = 0;
    ep->limit = limit;
    ep->ctls = xmalloc (sizeof (patchctl_t) * limit);
    ep->next = 1;

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
}

// }}}

#if 0

// state {{{

typedef struct refpool_s* refpool_t;

struct refpool_s {
    size_t capacity;
    void** ptrs;
};

struct patchctl_s {
    pthread_mutex_t mutex;
    pthread_t tid;
    struct ev_loop* loop;
    const char* addr;
    listener_t listener;

    patch_t patch;
    struct refpool_s arefs, prefs;
};

// TODO: proper reference handling wrt node memory management.

// }}}

// message interpreter {{{

typedef enum {
    OP_NOP = 0,
    OP_MKA, OP_MKP, OP_RMA, OP_RMP,
    OP_SRC, OP_SNK, OP_NSRC, OP_NSNK,
    OP_ADV,
    OP_ALA, OP_ALP,
    OP_MSG,
    OP_INFO,
    OP_INVALID
} opcode_t;

#define IS_PROC(op) ((op) <= OP_ADV)
#define IS_FUNC(op) (((op) > OP_ADV) && ((op) < OP_INVALID))

typedef enum {
    S_ERROR = -1,
    S_DONE = 0,
    S_INIT,
    S_PROC, S_PROC1, S_RET,
    S_FUNC,
    S_EXIT,

    S_MK, S_MK1,
    S_RM,
    S_CON, S_CON1, S_CON2,
    S_DIS, S_DIS1,
    S_ADV,

    S_AL,
    S_MSG, S_MSG1, S_MSG2, S_MSG3, S_MSG4, S_MSG5,
    S_INFO,

    S_NAT,
    S_INT,
    S_STR, S_STR1
} state_t;

#define SEL_BUF_SIZE 64
#define TAGS_BUF_SIZE 64

typedef struct {
    opcode_t op;
    size_t count; // number of procedure calls for op.
    size_t ref1, ref2;
    char sel[SEL_BUF_SIZE];
    char tags[TAGS_BUF_SIZE];
    word_t args[TAGS_BUF_SIZE];
    size_t tagp;

    // literal parsing stuff.
    size_t nat, nat2;
    ssize_t ntg;
    size_t len, clen;
    char* str;
} regs_t;

#define RETN_MAX 8

static int
interp (patchctl_t ctl UNUSED, proto_msg_t msg)
{
    const uint8_t* p = msg->payload;
    const uint8_t* end = p + msg->header.bytes;
    state_t state = S_INIT;
    regs_t regs = {
        .op = OP_INVALID,
        .count = 0,
        .ref1 = (size_t) -1, .ref2 = (size_t) -1,
        .tagp = 0,
        .nat = 0, .nat2 = 0,
        .ntg = 0,
        .len = 0,
        .clen = 0,
        .str = NULL,
    };
    state_t ret[RETN_MAX];
    size_t retn = 0;

    inline void push (state_t q)
    {
        if (retn == RETN_MAX)
            panic ("stack overflow");
        ret[retn++] = q;
    }

    inline state_t pop (void)
    {
        if (!retn)
            panic ("stack underflow");
        return ret[--retn];
    }

    while (state > S_DONE)
        switch (state)
        {
        // initial state {{{
        case S_INIT:
            if (p == end)
                { state = S_DONE; break; }
            uint8_t op = *p++;
            if (op >= (uint8_t) OP_INVALID)
                { state = S_ERROR; break; }
            regs.op = op;
            if (IS_PROC (op))
                state = S_PROC;
            else if (IS_FUNC (op))
                state = S_FUNC;
            else
                state = S_ERROR;
            break;
        // }}}

        // procedures {{{
        case S_PROC:
            push (S_PROC1);
            regs.nat = 0;
            state = S_NAT;
            break;

        case S_PROC1:
            regs.count = regs.nat;
            state = S_RET;
            // break;

        case S_RET:
            if (!regs.count)
                { state = S_INIT; break; }
            regs.count--;

            regs.nat = 0;
            state = S_NAT;
            switch (op)
            {
            case OP_MKA:
            case OP_MKP:
                push (S_MK); break;
            case OP_RMA:
            case OP_RMP:
                push (S_RM); break;
            case OP_SRC:
            case OP_SNK:
                push (S_CON); break;
            case OP_NSRC:
            case OP_NSNK:
                push (S_DIS); break;
            case OP_ADV:
                push (S_ADV); break;
            }
            break;
        // }}}

        // functions {{{
        case S_FUNC:
            regs.nat = 0;
            state = S_NAT;
            switch (op)
            {
            case OP_ALA:
            case OP_ALP:
                push (S_AL); break;
            case OP_MSG:
                push (S_MSG); break;
            case OP_INFO:
                state = S_INFO; break;
            }
            break;

        case S_EXIT:
            if (p != end)
            {
                state = S_ERROR;
                log_emit (LOG_DETAIL, "trailing bytes in a patchctl message");
            }
            else
                state = S_DONE;
            break;
        // }}}

        // MK* {{{
        case S_MK:
            push (S_MK1);
            regs.len = 0;
            state = S_STR;
            break;

        case S_MK1:
            log_emit (LOG_DETAIL, "MK%c %zu %s", (regs.op == OP_MKA)?'A':'P', regs.nat, regs.str);
            state = S_RET;
            break;
        // }}}

        // RM* {{{
        case S_RM:
            log_emit (LOG_DETAIL, "RM%c %zu", (regs.op == OP_RMA)?'A':'P', regs.nat);
            state = S_RET;
            break;
        // }}}

        // SRC, SNK {{{
        case S_CON:
            regs.ref1 = regs.nat;
            push (S_CON1);
            regs.nat = 0;
            state = S_NAT;
            break;

        case S_CON1:
            regs.nat2 = regs.nat;
            push (S_CON2);
            regs.nat = 0;
            state = S_NAT;
            break;

        case S_CON2:
            regs.ref2 = regs.nat;
            log_emit (LOG_DETAIL, "%s %zu %zu %zu", (regs.op == OP_SRC)?"SRC":"SNK", regs.ref1, regs.nat2, regs.ref2);
            state = S_RET;
            break;
        // }}}

        // NSRC, NSNK {{{
        case S_DIS:
            regs.ref1 = regs.nat;
            push (S_DIS1);
            regs.nat = 0;
            state = S_NAT;
            break;

        case S_DIS1:
            regs.nat2 = regs.nat;
            log_emit (LOG_DETAIL, "%s %zu %zu", (regs.op == OP_NSRC)?"NSRC":"NSNK", regs.ref1, regs.nat2);
            state = S_RET;
            break;
        // }}}

        // ADV {{{
        case S_ADV:
            log_emit (LOG_DETAIL, "ADV %zu", regs.nat);
            state = S_RET;
            break;
        // }}}

        // AL* {{{
        case S_AL:
            log_emit (LOG_DETAIL, "AL%c %zu", (op == OP_ALA)?'A':'P', regs.nat);
            state = S_EXIT;
            break;
        // }}}

        // MSG {{{
        case S_MSG:
            regs.ref1 = regs.nat;
            push (S_MSG1);
            state = S_STR;
            break;

        case S_MSG2:
            strncpy (regs.sel, regs.str, regs.len);
            push (S_MSG3);
            state = S_STR;
            break;

        case S_MSG3:
            strncpy (regs.tags, regs.str, regs.len);
            regs.tagp = 0;
            state = S_MSG4;
            //break;

        case S_MSG4:
            if (!regs.tags[regs.tagp])
            {
                log_emit (LOG_DETAIL, "MSG %zu %s %s", regs.ref1, regs.sel, regs.tags);
                state = S_EXIT;
                break;
            }

            push (S_MSG5);
            switch (regs.tags[regs.tagp])
            {
            case 'n':
                regs.nat = 0;
                state = S_NAT;
                break;
            case 'i':
                regs.ntg = 0;
                state = S_INT;
                break;
            case 's':
                state = S_STR;
                break;
            default:
                state = S_ERROR;
                break;
            }
            break;

        case S_MSG5:
            switch (regs.tags[regs.tagp])
            {
            case 'n':
                regs.args[regs.tagp].n = regs.nat;
                break;

            }
            regs.tagp++;
            state = S_MSG4;
            break;
        // }}}

        // naturals {{{
        case S_NAT:
            if (p == end)
                { state = S_ERROR; break; }
            else
            {
                uint8_t b = *p;
                regs.nat <<= 7;
                regs.nat |= b & 0x7F;
                if (!(b & 0x80))
                    state = pop ();
                break;
            }
        // }}}

        // integers {{{
        case S_INT:
            if (p == end)
            { state = S_ERROR; break; }
            else
            {
                uint8_t b = *p;
                regs.ntg <<= 7;
                regs.ntg |= b & 0x7F;
                if (!(b & 0x80))
                {
                    state = pop ();
                    if (regs.ntg & 1)
                        regs.ntg = -(regs.ntg>>1) - 1;
                    else
                        regs.ntg >>= 1;
                }
            }
        // }}}

        // strings {{{
        case S_STR:
            if (p == end)
                { state = S_ERROR; break; }
            uint8_t b = *p;
            regs.len <<= 7;
            regs.len |= b & 0x7F;
            if (!(b & 0x80))
            {
                regs.str = xmalloc (regs.len + 1);
                memset (regs.str, 0, regs.len + 1);
                regs.clen = 0;
                state = S_STR1;
            }
            break;

        case S_STR1:
            if (regs.clen == regs.len)
                { state = pop (); break; }
            if (p == end)
                { state = S_ERROR; break; }
            regs.str[regs.clen++] = *p++;
            break;
        // }}}

        default:
            state = S_ERROR;
            break;
        }

    return state == S_DONE;
}

// }}}

// behavior {{{

static void
ctl_accept (listener_t listener, peer_t peer)
{
    patchctl_t ctl = listener_get_state (listener);
    peer_set_state (peer, ctl);
}

static void
ctl_message (peer_t peer, proto_msg_t msg)
{
    patchctl_t ctl = peer_get_state (peer);

    pthread_mutex_lock (&(ctl->mutex));
    if (interp (ctl, msg) == -1)
        log_emit (LOG_DETAIL, "malformed message arrived at %s", ctl->addr);
    pthread_mutex_unlock (&(ctl->mutex));
}

static void
ctl_shutdown (peer_t peer)
{
    patchctl_t ctl UNUSED = peer_get_state (peer);
}

static struct listener_beh_s ctl_beh = {
    .on_accept = ctl_accept,
    .peer_beh = {
        .on_message = ctl_message,
        .on_shutdown = ctl_shutdown
    }
};

// }}}

// front-end {{{

static void*
patchctl_thread (void* arg)
{
    patchctl_t ctl = arg;
    pthread_mutex_lock (&(ctl->mutex));

    ctl->loop = ev_loop_new (EVFLAG_AUTO);
    if (!ctl->loop)
        panic ("ev_loop_new() failed in patchctl thread");

    ctl->listener = listener_create (ctl->loop, &ctl_beh, ctl, ctl->addr);

    pthread_mutex_unlock (&(ctl->mutex));
    ev_loop (ctl->loop, 0);

    return NULL;
}

patchctl_t
patchctl_create (const char* addr, size_t nworkers)
{
    patchctl_t ctl = xmalloc (sizeof (struct patchctl_s));

    int res = pthread_mutex_init (&(ctl->mutex), NULL);
    if (res)
        panic ("pthread_mutex_init() returned %d", res);

    ctl->addr = addr;
    ctl->patch = patch_create (nworkers);
    ctl->arefs.capacity = 0;
    ctl->arefs.ptrs = NULL;
    ctl->prefs = ctl->arefs;

    res = pthread_create (&(ctl->tid), NULL, patchctl_thread, ctl);
    if (res)
        panic ("pthread_create() returned %d", res);

    return ctl;
}

void
patchctl_destroy (patchctl_t ctl)
{
    pthread_mutex_lock (&(ctl->mutex));

    listener_destroy (ctl->listener);

    pthread_mutex_unlock (&(ctl->mutex));

    pthread_join (ctl->tid, NULL);

    int res = pthread_mutex_destroy (&(ctl->mutex));
    if (res)
        panic ("pthread_mutex_destroy() returned %d", res);

    free (ctl);
}

// }}}

#endif

// vim:fdm=marker:
