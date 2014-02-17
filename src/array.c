
/*
 * koar/array.c
 * copyright (c) 2014 Frano Perleta
 */

#include "patchvm.h"
#include "array.h"

// create {{{

array_t
array_create (size_t size)
{
    samp_t* xs = xmalloc (sizeof (samp_t) * size);
    array_t arr = xmalloc (sizeof (struct array_s));

    int res = pthread_mutex_init (&(arr->mutex), NULL);
    if (res)
        panic ("pthread_mutex_init() returned %d", res);

    arr->refcount = 1;
    arr->size = size;
    arr->xs = xs;

    return arr;
}

// }}}

// destroy {{{

static void
array_destroy (array_t arr)
{
    int res = pthread_mutex_destroy (&(arr->mutex));
    if (res)
        panic ("pthread_mutex_destroy() returned %d", res);
    free (arr);
}

// }}}

// acquire {{{

array_t
array_acquire (array_t arr)
{
    pthread_mutex_lock (&(arr->mutex));
    arr->refcount++;
    pthread_mutex_unlock (&(arr->mutex));
    return arr;
}

// }}}

// release {{{

void
array_release (array_t arr)
{
    pthread_mutex_lock (&(arr->mutex));
    int last = !(--arr->refcount);
    pthread_mutex_unlock (&(arr->mutex));
    if (last)
        array_destroy (arr);
}

// }}}

// make {{{

void
PATCHVM_array_make (patchvm_t vm, instr_t instr)
{
    size_t size = instr->args[1].nat;
    reg_t val = { .tag = T_ARRAY, .arr = array_create (size) };
    patchvm_set (vm, instr->args[0].reg, val);
}

// }}}

// const {{{

void
array_const (array_t arr, samp_t x)
{
    size_t i;
    samp_t* xs = arr->xs;

    for (i = 0; i < arr->size; i++)
        xs[i] = x;
}

void
PATCHVM_array_const (patchvm_t vm, instr_t instr)
{
    array_t arr = patchvm_get (vm, instr->args[0].reg).arr;
    double x0 = instr->args[1].dbl;
    array_const (arr, x0);
}

// }}}

// normalize {{{

void
array_normalize (array_t arr, samp_t amp)
{
    size_t i;
    samp_t* xs = arr->xs;
    samp_t m = 0;

    for (i = 0; i < arr->size; i++)
        if (m < fabs (xs[i]))
            m = fabs (xs[i]);

    if (m <= 0)
        return;

    for (i = 0; i < arr->size; i++)
        xs[i] /= m / amp;
}

void
PATCHVM_array_normalize (patchvm_t vm, instr_t instr)
{
    array_t arr = patchvm_get (vm, instr->args[0].reg).arr;
    double amp = instr->args[1].dbl;
    array_normalize (arr, amp);
}

// }}}

// dc {{{

void
array_dc (array_t arr, samp_t offset)
{
    size_t i;
    samp_t* xs = arr->xs;
    samp_t sum = 0;

    for (i = 0; i < arr->size; i++)
        sum += xs[i];

    samp_t dx = offset - sum / arr->size;

    for (i = 0; i < arr->size; i++)
        xs[i] += dx;
}

void
PATCHVM_array_dc (patchvm_t vm, instr_t instr)
{
    array_t arr = patchvm_get (vm, instr->args[0].reg).arr;
    double offset = instr->args[1].dbl;
    array_dc (arr, offset);
}

// }}}

// partial {{{

void
array_partial (array_t arr, samp_t amp, unsigned index, double phase)
{
    size_t i;
    samp_t* xs = arr->xs;

    double x, dx = index / (double) arr->size;

    for (i = 0, x = phase; i < arr->size; i++, x += dx)
        xs[i] = amp * sin (2 * M_PI * x);
}

void
PATCHVM_array_partial (patchvm_t vm, instr_t instr)
{
    array_t arr = patchvm_get (vm, instr->args[0].reg).arr;
    double amp = instr->args[1].dbl;
    unsigned index = instr->args[2].nat;
    double phase = instr->args[3].dbl;
    array_partial (arr, amp, index, phase);
}

// }}}

// vim:fdm=marker:
