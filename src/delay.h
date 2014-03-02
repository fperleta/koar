
/*
 * koar/delay.h
 * copyright (c) 2014 Frano Perleta
 */

#ifndef KOAR_DELAY_H
#define KOAR_DELAY_H

#include <pthread.h>
#include "buf.h"
#include "patch.h"
#include "defs.h"

// types {{{

typedef struct delay_s* delay_t;

struct delay_s {
    pthread_mutex_t mutex;
    size_t refcount;
    size_t size;
    size_t head;
    patch_stamp_t stamp;
    samp_t* xs;
};

// }}}

// basic operations {{{

extern delay_t delay_create (size_t, patch_stamp_t); // refcount = 1
extern delay_t delay_acquire (delay_t);
extern void delay_release (delay_t);

MACRO void delay_lock (delay_t del)
{ pthread_mutex_lock (&(del->mutex)); }

MACRO void delay_unlock (delay_t del)
{ pthread_mutex_unlock (&(del->mutex)); }

// }}}

// pushing {{{

extern void delay_push (delay_t, patch_stamp_t, size_t, const samp_t*);

// }}}

// tapping {{{

extern void delay_qtap (delay_t, patch_stamp_t, size_t, buf_t);

// }}}

#endif /* koar/delay.h */

// vim:fdm=marker:
