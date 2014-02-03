
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
patch_activate (patch_t p, anode_t an)
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

void
patch_tick (patch_t p)
{
    pthread_mutex_lock (&(p->mutex));

    if (p->working || p->count)
    {
        if (p->count)
            pthread_cond_broadcast (&(p->nonempty));
        pthread_cond_wait (&(p->empty), &(p->mutex));
    }

    p->now += p->delta;

    pthread_mutex_unlock (&(p->mutex));
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

    an->stamp = (patch_stamp_t) -1;
    an->sources = an->waiting = 0;

    size_t i;
    for (i = 0; i < info->ins + info->outs; i++)
        an->refs[i] = NULL;

    pthread_mutex_lock (&(an->mutex)); // is this really necessary?
    info->init (p, an);
    pthread_mutex_unlock (&(an->mutex));

    return an;
}

void
anode_destroy (patch_t p UNUSED, anode_t an)
{
    int res = pthread_mutex_destroy (&(an->mutex));
    if (res)
        panic ("pthread_mutex_destroy() returned %d", res);
    free (an);
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

    pn->stamp = (unsigned) -1;
    pn->writers = pn->written = pn->nreaders = 0;
    pn->readers = NULL;

    return pn;
}

void
pnode_destroy (pnode_t pn)
{
    int res = pthread_mutex_destroy (&(pn->mutex));
    if (res)
        panic ("pthread_mutex_destroy() returned %d", res);
    free (pn);
}

patch_datum_t
pnode_read (pnode_t pn, patch_stamp_t now)
{
    pthread_mutex_lock (&(pn->mutex));

    if (pn->stamp != now)
    {
        pn->written = 0;
        pn->state = pn->info->neutral;
    }

    patch_datum_t x = pn->state;

    pthread_mutex_unlock (&(pn->mutex));

    return x;
}

static void
input_ready (patch_t p, anode_t an, patch_stamp_t now)
{
    pthread_mutex_lock (&(an->mutex));

    if (an->stamp != now)
        an->waiting = an->sources;

    if (!(--an->waiting))
        patch_activate (p, an);

    pthread_mutex_unlock (&(an->mutex));
}

void
pnode_write (patch_t p, pnode_t pn, patch_datum_t x, patch_stamp_t now)
{
    pthread_mutex_lock (&(pn->mutex));

    if (pn->stamp == now)
    {
        pn->written++;
        pn->state = pn->info->combine (pn->state, x);
    }
    else
    {
        pn->stamp = now;
        pn->written = 1;
        pn->state = x;
    }

    log_emit (LOG_DEBUG, "pnode_write() written %zu/%zu", pn->written, pn->writers);

    if (pn->written == pn->writers)
    {
        if (pn->nreaders == 1)
            input_ready (p, pn->reader, now);
        else
        {
            size_t i;
            for (i = 0; i < pn->nreaders; i++)
                input_ready (p, pn->readers[i], now);
        }
    }

    pthread_mutex_unlock (&(pn->mutex));
}

// }}}

// vim:fdm=marker:
