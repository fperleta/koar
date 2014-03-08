
/*
 * koar/patch.h
 * copyright (c) 2013 Frano Perleta
 */

#ifndef KOAR_PATCH_H
#define KOAR_PATCH_H

#define _GNU_SOURCE
#include <pthread.h>
#include "buf.h"
#include "defs.h"

// documentation {{{
/*******************************************************************************

locking protocol
================

api calls:

* `patch_destroy`: patch unlocked
* `patch_root`: patch unlocked, anode unlocked
* `patch_unroot`: patch unlocked, anode unlocked
* `patch_tick`: patch unlocked
* `anode_create`: patch unlocked
* `anode_acquire`: anode unlocked
* `anode_release`: anode unlocked
* `anode_source`: anode unlocked, pnode unlocked
* `anode_sink`: anode unlocked, pnode unlocked
* `pnode_acquire`: pnode unlocked
* `pnode_release`: pnode unlocked
* `pnode_read`: pnode unlocked
* `pnode_write`: patch unlocked, pnode unlocked
* `pnode_dont_write`: patch unlocked, pnode unlocked

callbacks:

* `anode_init_t`: patch unlocked, anode effectively locked
* `anode_exit_t`: anode locked
* `anode_tick_t`: patch unlocked, anode locked
* `pnode_combine_t`: patch unlocked, pnode locked
* `pnode_dispose_t`: pnode locked

*******************************************************************************/
// }}}

// macros {{{

#define PATCH_ALIGNED __attribute__((aligned (8)))

#define PATCH_QUEUE_SIZE 128
#define PATCH_ROOTS_INCR 64

// }}}

// typedefs {{{

typedef struct patch_s* patch_t;
typedef struct anode_s* anode_t;
typedef struct ainfo_s* ainfo_t;
typedef struct pnode_s* pnode_t;
typedef struct pinfo_s* pinfo_t;

typedef union {
    void* p;
    buf_t b;
    double d;
} patch_datum_t;

typedef size_t patch_stamp_t;

// }}}

// patches {{{

struct patch_s {
    // patch mutex
    pthread_mutex_t mutex;

    // free worker queue
    pthread_cond_t nonempty;
    // signalled when the activation queue is exhausted
    pthread_cond_t empty;

    // worker control
    int shutdown;
    size_t nworkers, working;

    // time
    size_t delta;
    patch_stamp_t now;

    // bufpool
    bufpool_t bufpool;

    // initial nodes
    size_t nroots, roots_cap;
    anode_t* roots;

    // activation queue
    size_t head, tail, count;
    anode_t queue[PATCH_QUEUE_SIZE];
} PATCH_ALIGNED;

extern int patch_init (patch_t, size_t);
extern patch_t patch_create (size_t);
extern void patch_destroy (patch_t);

// mutual exclusion {{{

MACRO void patch_lock (patch_t p)
{ pthread_mutex_lock (&(p->mutex)); }

MACRO void patch_unlock (patch_t p)
{ pthread_mutex_unlock (&(p->mutex)); }

// }}}

extern void patch_root (patch_t, anode_t);
extern void patch_unroot (patch_t, anode_t);
extern void patch_advance (patch_t, size_t);

// patch protocol:
// 1. create a patch;
// 2. repeat:
//      1. create anodes, send messages, etc;
//      2. set delta;
//      3. activate the initial anodes;
//      4. call patch_tick().
// 3. destroy the patch.

// }}}

// active nodes {{{

typedef void (*anode_init_t) (patch_t, anode_t);
typedef void (*anode_exit_t) (anode_t);
typedef void (*anode_tick_t) (patch_t, anode_t, patch_stamp_t, size_t);

struct anode_s {
    pthread_mutex_t mutex;
    ainfo_t info;
    size_t refcount;
    patch_t root_patch;
    patch_stamp_t stamp;
    size_t sources, waiting;
    pnode_t refs[];
} PATCH_ALIGNED;

struct ainfo_s {
    const char* name;
    anode_init_t init;
    anode_exit_t exit;
    anode_tick_t tick;
    size_t ins, outs, size;
} PATCH_ALIGNED;

extern anode_t anode_create (patch_t, ainfo_t); // refcount = 1
extern anode_t anode_acquire (anode_t);
extern void anode_release (anode_t);

// mutual exclusion {{{

MACRO void anode_lock (anode_t an)
{ pthread_mutex_lock (&(an->mutex)); }

MACRO void anode_unlock (anode_t an)
{ pthread_mutex_unlock (&(an->mutex)); }

// }}}

// accessors {{{

// WARNING: calling code is responsible for mutual exclusion.

static inline pnode_t
anode_get_source (anode_t an, size_t i)
{
#ifdef DEBUG
    if (i >= an->info->ins)
        panic ("index out of bounds");
#endif /* DEBUG */
    return an->refs[i];
}

static inline pnode_t
anode_get_sink (anode_t an, size_t i)
{
#ifdef DEBUG
    if (i >= an->info->outs)
        panic ("index out of bounds");
#endif /* DEBUG */
    return an->refs[an->info->ins + i];
}

static inline void*
anode_state (anode_t an)
{
    return (void*) (an->refs + (an->info->ins + an->info->outs));
}

// }}}

extern void anode_source (anode_t, size_t, pnode_t);
extern void anode_sink (anode_t, size_t, pnode_t);

// }}}

// passive nodes {{{

typedef patch_datum_t (*pnode_combine_t) (patch_t, patch_datum_t, patch_datum_t);
typedef patch_datum_t (*pnode_pass_t) (patch_t, patch_datum_t);
typedef void (*pnode_dispose_t) (patch_datum_t);

struct pnode_s {
    pthread_mutex_t mutex;
    pinfo_t info;
    size_t refcount;
    patch_stamp_t stamp; // -1 when uninitialized
    size_t writers, written;
    size_t nreaders, toread;
    union {
        anode_t reader;
        anode_t* readers;
    };
    patch_datum_t state;
    patch_t patch;
} PATCH_ALIGNED;

struct pinfo_s {
    patch_datum_t neutral;
    pnode_combine_t combine;
    pnode_pass_t pass;
    pnode_dispose_t dispose;
} PATCH_ALIGNED;

extern pnode_t pnode_create (patch_t, pinfo_t); // refcount = 1
extern pnode_t pnode_acquire (pnode_t);
extern void pnode_release (pnode_t);

extern patch_datum_t pnode_read (pnode_t, patch_stamp_t);
extern void pnode_write (patch_t, pnode_t, patch_datum_t, patch_stamp_t);
extern void pnode_dont_write (patch_t, pnode_t, patch_stamp_t);

// }}}

#endif /* koar/patch.h */

// vim:fdm=marker:
