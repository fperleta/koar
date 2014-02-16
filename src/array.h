
/*
 * koar/array.h
 * copyright (c) 2014 Frano Perleta
 */

#ifndef KOAR_ARRAY_H
#define KOAR_ARRAY_H

#include <pthread.h>
#include "buf.h"
#include "defs.h"

// types {{{

typedef struct array_s* array_t;

struct array_s {
    pthread_mutex_t mutex;
    size_t refcount;
    size_t size;
    samp_t* xs;
};

// }}}

// basic operations {{{

extern array_t array_create (size_t); // refcount = 1
extern array_t array_acquire (array_t);
extern void array_release (array_t);

MACRO void array_lock (array_t arr)
{ pthread_mutex_lock (&(arr->mutex)); }

MACRO void array_unlock (array_t arr)
{ pthread_mutex_unlock (&(arr->mutex)); }

// }}}

// mutation {{{

extern void array_const (array_t, samp_t);
extern void array_normalize (array_t, samp_t);
extern void array_partial (array_t, samp_t, unsigned, double); // amp, freq, phase

// }}}

#endif /* koar/array.h */

// vim:fdm=marker:
