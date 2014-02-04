
/*
 * koar/patch.c
 * copyright (c) 2013 Frano Perleta
 */

#include "patch.h"

// documentation {{{
/*******************************************************************************

internal invariants
===================

the patch mutex must be locked whenever the contents of the patch_s structure
are accessed.

the condition variable `nonempty` is used to signal to any idle worker threads
that the activation queue might contain items for them to process.

the condition variable `empty` is used to signal to the controlling thread that
there are no more nodes to process. this means that the tick is complete, and
the patch time can be incremented by the tick delta.

the variable `shutdown` is used to notify the worker threads that the patch is
about to be destroyed.

`nworkers` and `working` are used to track the total number of workers and the
number of workers presently processing a node, respectively.

the variable `delta` holds the duration of the current tick as the number
sample periods. the variable `now` holds the absolute time at the beginning of
the current tick.

the patch goes through two alternating phases:

- control phase: this is the initial phase. all the worker threads are waiting,
so the patch can be safely manipulated by the controlling thread. nodes may be
created, destroyed, and their state may be modified.  this phase is resumed
each time the `empty` condition variable is signalled.

- processing phase: the first time the `nonempty` condition variable is
signalled while in the control phase, the processing phase is entered.  during
this phase there is a notion of a current tick, defined by the values of `now`
and `delta` at the moment that `nonempty` is first signalled. after activating
all the initial nodes, the controlling process should call `patch_tick`, which
returns control only after the processing phase is over.

resource management
===================

the patch_s structure itself has no permanent reference to any node. the
activation queue contains pointers to active nodes, but only during the
processing phase, when no nodes may be created or destroyed, so this is
irrelevant.

since the patch is an acyclic graph, a simple reference-counting system is
used for nodes.

for passive nodes, both readers and writers are counted, as well as any
external references held by the controlling thread.

when an anode writes a buf to a pnode, it should ensure that the pnode has
complete control over it, ie. that the refcount is 1, and the buf isn't
modified afterwards.

bufs read from source pnodes must never be modified, and must be released
when no longer needed.

*******************************************************************************/
// }}}

// worker threads {{{

static void
patch_free (patch_t p)
{
    int res = pthread_cond_destroy (&(p->nonempty));
    if (res)
        panic ("pthread_cond_destroy() returned %d", res);
    res = pthread_mutex_destroy (&(p->mutex));
    if (res)
        panic ("pthread_mutex_destroy() returned %d", res);
    bufpool_destroy (p->bufpool);
    free (p);
}

static void*
worker (void* arg)
{
    patch_t p = (patch_t) arg;
    pthread_t self = pthread_self ();

    pthread_detach (self);

    pthread_mutex_lock (&(p->mutex));

    while (!p->shutdown)
    {
        if (!p->count)
        {
            log_emit (LOG_DEBUG, "patch worker %d waiting for activations...", self);
            int res = pthread_cond_wait (&(p->nonempty), &(p->mutex));
            if (res)
                panic ("pthread_cond_wait() returned %d", res);
            log_emit (LOG_DEBUG, "patch worker %d wakeup", self);
        }

        if (p->shutdown)
            break;
        if (!p->count)
            continue;

        anode_t an = p->queue[p->head];
        p->head = (p->head + 1) % PATCH_QUEUE_SIZE;
        p->count--;
        patch_stamp_t now = p->now;
        size_t delta = p->delta;

        p->working++;
        pthread_mutex_unlock (&(p->mutex));
        pthread_mutex_lock (&(an->mutex));
        log_emit (LOG_DEBUG, "patch worker %d processing anode %p at time %zu", self, an, now);
        an->info->tick (p, an, now, delta);
        pthread_mutex_unlock (&(an->mutex));
        pthread_mutex_lock (&(p->mutex));
        p->working--;

        if (!p->working && !p->count)
        {
            log_emit (LOG_DEBUG, "patch worker %d broadcasting empty", self);
            pthread_cond_broadcast (&(p->empty));
        }
    }

    p->nworkers--;
    int done = !p->nworkers;

    pthread_mutex_unlock (&(p->mutex));

    if (done)
        patch_free (p);

    return NULL;
}

// }}}

// patches {{{

int
patch_init (patch_t p, size_t nworkers)
{
    int res = pthread_mutex_init (&(p->mutex), NULL);
    if (res)
        return res;

    res = pthread_cond_init (&(p->nonempty), NULL);
    if (res)
        return res;

    res = pthread_cond_init (&(p->empty), NULL);
    if (res)
        return res;

    p->shutdown = 0;
    p->nworkers = nworkers;
    p->working = 0;

    p->delta = BUF_SAMPLES;
    p->now = 0;

    p->bufpool = bufpool_create ();

    p->nroots = p->roots_cap = 0;
    p->roots = NULL;

    p->head = p->tail = p->count = 0;

    size_t i;
    for (i = 0; i < nworkers; i++)
    {
        pthread_t tid;
        res = pthread_create (&tid, NULL, worker, p);
        if (res)
            panic ("pthread_create() returned %d", res);
    }

    return 0;
}

patch_t
patch_create (size_t nworkers)
{
    patch_t p = xmalloc (sizeof (struct patch_s));
    int res = patch_init (p, nworkers);
    if (res)
    {
        free (p);
        panic ("patch_init() returned %d", res);
    }
    return p;
}

void
patch_destroy (patch_t p)
{
    pthread_mutex_lock (&(p->mutex));
    p->shutdown = 1;
    pthread_mutex_unlock (&(p->mutex));
}

void
patch_root (patch_t p, anode_t an)
{
    patch_lock (p);

    if (p->nroots + 1 >= p->roots_cap)
    {
        p->roots_cap += PATCH_ROOTS_INCR;
        p->roots = xrealloc (p->roots, p->roots_cap * sizeof (anode_t));
    }

    p->roots[p->nroots++] = an;

    patch_unlock (p);
}

void
patch_unroot (patch_t p, anode_t an)
{
    patch_lock (p);

    size_t i;
    for (i = 0; i < p->nroots; i++)
        if (p->roots[i] == an)
        {
            p->roots[i] = p->roots[--p->nroots];
            goto done;
        }

done:
    patch_unlock (p);
}

static void
activate (patch_t p, anode_t an)
{
    pthread_mutex_lock (&(p->mutex));

    if (p->count == PATCH_QUEUE_SIZE)
        panic ("patch queue overflow");

    p->queue[p->tail] = an;
    p->tail = (p->tail + 1) % PATCH_QUEUE_SIZE;
    p->count++;

    pthread_cond_signal (&(p->nonempty));

    pthread_mutex_unlock (&(p->mutex));
}

static void
patch_tick (patch_t p, size_t delta)
{
    /* activate the roots */ {
        patch_lock (p);

        p->delta = delta;

        if (p->nroots > PATCH_QUEUE_SIZE)
            panic ("patch queue overflow");

        p->head = 0;
        p->tail = p->nroots;
        p->count = p->nroots;

        size_t i;
        for (i = 0; i < p->nroots; i++)
            p->queue[i] = p->roots[i];

        patch_unlock (p);
    }

    // wake up the workers
    pthread_cond_signal (&(p->nonempty));

    /* wait for the workers */ {
        patch_lock (p);

        if (p->working || p->count)
        {
            if (p->count)
                pthread_cond_broadcast (&(p->nonempty));
            pthread_cond_wait (&(p->empty), &(p->mutex));
        }

        p->now += p->delta;

        patch_unlock (p);
    }
}

void
patch_advance (patch_t p, size_t delta)
{
    size_t t, dt;

    for (t = 0; t < delta; t += dt)
    {
        dt = (delta - t < BUF_SAMPLES)? delta - t : BUF_SAMPLES;
        patch_tick (p, dt);
    }
}

// }}}

// active nodes {{{

anode_t
anode_create (patch_t p, ainfo_t info)
{
    size_t bytes = __builtin_offsetof (struct anode_s, refs)
                 + (info->ins + info->outs) * sizeof (pnode_t)
                 + info->size;

    anode_t an = xmalloc (bytes);
    an->info = info;

    int res = pthread_mutex_init (&(an->mutex), NULL);
    if (res)
        panic ("pthread_mutex_init() returned %d", res);

    an->refcount = 1;
    an->root_patch = NULL;
    an->stamp = (patch_stamp_t) -1;
    an->sources = an->waiting = 0;

    size_t i;
    for (i = 0; i < info->ins + info->outs; i++)
        an->refs[i] = NULL;

    info->init (p, an);

    if (!info->ins)
    {
        patch_root (p, an);
        an->root_patch = p;
    }

    return an;
}

static void
anode_destroy (anode_t an)
{
    pthread_mutex_lock (&(an->mutex));
    an->info->exit (an);
    if (an->root_patch)
        patch_unroot (an->root_patch, an);
    pthread_mutex_unlock (&(an->mutex));

    int res = pthread_mutex_destroy (&(an->mutex));
    if (res)
        panic ("pthread_mutex_destroy() returned %d", res);
    free (an);
}

anode_t
anode_acquire (anode_t an)
{
    pthread_mutex_lock (&(an->mutex));
    an->refcount++;
    pthread_mutex_unlock (&(an->mutex));
    return an;
}

void
anode_release (anode_t an)
{
    pthread_mutex_lock (&(an->mutex));
    int last_ref = !(--an->refcount);
    pthread_mutex_unlock (&(an->mutex));
    if (last_ref)
        anode_destroy (an);
}

static void
add_reader (pnode_t pn, anode_t an)
{
    pthread_mutex_lock (&(pn->mutex));

    size_t old = pn->nreaders++;

    if (old == 0)
        pn->reader = an;
    else if (old == 1)
    {
        anode_t r = pn->reader;
        pn->readers = xmalloc (2 * sizeof (anode_t));
        pn->readers[0] = r;
        pn->readers[1] = an;
    }
    else
    {
        pn->readers = xrealloc (pn->readers, pn->nreaders * sizeof (anode_t));
        pn->readers[old] = an;
    }

    pthread_mutex_unlock (&(pn->mutex));

    pnode_acquire (pn);

}

static void
remove_reader (pnode_t pn, anode_t an)
{
    pthread_mutex_lock (&(pn->mutex));

    size_t old = pn->nreaders;

    if ((old == 1) && (pn->reader == an))
    {
        pn->nreaders--;
        pn->reader = NULL;
    }
    else if (old > 1)
    {
        size_t i;
        for (i = 0; i < old; i++)
            if (pn->readers[i] == an)
            {
                if (--pn->nreaders == 1)
                {
                    anode_t r = pn->readers[1-i];
                    pn->reader = r;
                    break;
                }
                for (++i; i < old; i++)
                    pn->readers[i-1] = pn->readers[i];
                break;
            }
    }

    pthread_mutex_unlock (&(pn->mutex));

    pnode_release (pn);
}

void
anode_source (anode_t an, size_t i, pnode_t pn)
{
    pthread_mutex_lock (&(an->mutex));

    if (i >= an->info->ins)
        panic ("anode_source() index out of bounds");
    size_t j = i;

    if (an->refs[j])
    {
        pnode_t old = an->refs[j];
        if (old == pn)
            goto done;
        if (old)
        {
            remove_reader (old, an);
            an->sources--;
        }
    }

    an->refs[j] = pn;
    if (pn)
    {
        add_reader (pn, an);
        an->sources++;
    }

done:
    pthread_mutex_unlock (&(an->mutex));
}

static void
add_writer (pnode_t pn)
{
    pthread_mutex_lock (&(pn->mutex));
    pn->writers++;
    pthread_mutex_unlock (&(pn->mutex));
}

static void
remove_writer (pnode_t pn)
{
    pthread_mutex_lock (&(pn->mutex));
    pn->writers--;
    pthread_mutex_unlock (&(pn->mutex));
}

void
anode_sink (anode_t an, size_t i, pnode_t pn)
{
    pthread_mutex_lock (&(an->mutex));

    if (i >= an->info->outs)
        panic ("anode_sink() index out of bounds");
    size_t j = an->info->ins + i;

    if (an->refs[j])
    {
        pnode_t old = an->refs[j];
        if (old == pn)
            goto done;
        remove_writer (pn);
    }

    an->refs[j] = pn;
    if (pn)
        add_writer (pn);

done:
    pthread_mutex_unlock (&(an->mutex));
}

// }}}

// passive nodes {{{

pnode_t
pnode_create (pinfo_t info)
{
    pnode_t pn = xmalloc (sizeof (struct pnode_s));
    pn->info = info;

    int res = pthread_mutex_init (&(pn->mutex), NULL);
    if (res)
        panic ("pthread_mutex_init() returned %d", res);

    pn->refcount = 1;
    pn->stamp = (unsigned) -1;
    pn->writers = pn->written = pn->nreaders = pn->toread = 0;
    pn->readers = NULL;

    return pn;
}

static void
pnode_destroy (pnode_t pn)
{
    int res = pthread_mutex_destroy (&(pn->mutex));
    if (res)
        panic ("pthread_mutex_destroy() returned %d", res);
    free (pn);
}

pnode_t
pnode_acquire (pnode_t pn)
{
    pthread_mutex_lock (&(pn->mutex));
    pn->refcount++;
    pthread_mutex_unlock (&(pn->mutex));
    return pn;
}

void
pnode_release (pnode_t pn)
{
    pthread_mutex_lock (&(pn->mutex));
    int last_ref = !(--pn->refcount);
    pthread_mutex_unlock (&(pn->mutex));
    if (last_ref)
        pnode_destroy (pn);
}

patch_datum_t
pnode_read (pnode_t pn, patch_stamp_t now)
{
    pthread_mutex_lock (&(pn->mutex));

    if (!pn->toread)
        panic ("pnode_read() underflow");

    if (pn->stamp != now)
    {
        pn->written = 0;
        pn->state = pn->info->neutral;
        pn->toread = pn->nreaders;
    }

    patch_datum_t x = pn->state;
    if (!(--pn->toread))
    {
        pn->info->dispose (pn->state);
        pn->state = pn->info->neutral;
    }

    pthread_mutex_unlock (&(pn->mutex));

    return pn->info->pass (x);
}

static void
input_ready (patch_t p, anode_t an, patch_stamp_t now)
{
    pthread_mutex_lock (&(an->mutex));

    if (an->stamp != now)
    {
        an->waiting = an->sources;
        an->stamp = now;
    }

    if (!(--an->waiting))
        activate (p, an);

    pthread_mutex_unlock (&(an->mutex));
}

static void
check_written (patch_t p, pnode_t pn, patch_stamp_t now)
{
    if (pn->written < pn->writers)
        return;

    pn->toread = pn->nreaders;
    if (!(pn->toread))
    {
        pn->info->dispose (pn->state);
        pn->state = pn->info->neutral;
    }

    if (pn->nreaders == 1)
        input_ready (p, pn->reader, now);
    else
    {
        size_t i;
        for (i = 0; i < pn->nreaders; i++)
            input_ready (p, pn->readers[i], now);
    }
}

void
pnode_write (patch_t p, pnode_t pn, patch_datum_t x, patch_stamp_t now)
{
    pthread_mutex_lock (&(pn->mutex));

    if (pn->stamp == now)
    {
        pn->written++;
        pn->state = pn->info->combine (p, pn->state, x);
    }
    else
    {
        pn->stamp = now;
        pn->written = 1;
        pn->state = x;
    }

    log_emit (LOG_DEBUG, "pnode_write() written %zu/%zu", pn->written, pn->writers);

    check_written (p, pn, now);

    pthread_mutex_unlock (&(pn->mutex));
}

void
pnode_dont_write (patch_t p, pnode_t pn, patch_stamp_t now)
{
    pthread_mutex_lock (&(pn->mutex));

    if (pn->stamp == now)
        pn->written++;
    else
    {
        pn->stamp = now;
        pn->written = 1;
        pn->state = pn->info->neutral;
    }

    log_emit (LOG_DEBUG, "pnode_dont_write() written %zu/%zu", pn->written, pn->writers);

    check_written (p, pn, now);

    pthread_mutex_unlock (&(pn->mutex));
}

// }}}

// vim:fdm=marker:
