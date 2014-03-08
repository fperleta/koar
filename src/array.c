
/*
 * koar/array.c
 * copyright (c) 2014 Frano Perleta
 */

#include <sndfile.h>
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

    /* initialize to zero */ {
        size_t i;
        for (i = 0; i < size; i++)
            xs[i] = 0;
    }

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
    if (arr->xs)
        free (arr->xs);
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
    array_lock (arr);
    array_const (arr, x0);
    array_unlock (arr);
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
    array_lock (arr);
    array_normalize (arr, amp);
    array_unlock (arr);
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
    array_lock (arr);
    array_dc (arr, offset);
    array_unlock (arr);
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
        xs[i] += amp * sin (2 * M_PI * x);
}

void
PATCHVM_array_partial (patchvm_t vm, instr_t instr)
{
    array_t arr = patchvm_get (vm, instr->args[0].reg).arr;
    double amp = instr->args[1].dbl;
    unsigned index = instr->args[2].nat;
    double phase = instr->args[3].dbl;
    array_lock (arr);
    array_partial (arr, amp, index, phase);
    array_unlock (arr);
}

// }}}

// ghw {{{

void
array_ghw (array_t arr, double alpha, double beta)
{
    size_t i, len = arr->size;
    double x, dx = 2 * M_PI / (len - 1.0);
    samp_t* xs = arr->xs;

    for (i = 0, x = -M_PI; i < len; i++, x += dx)
        xs[i] *= alpha - beta * cos (x);
}

void
PATCHVM_array_ghw (patchvm_t vm, instr_t instr)
{
    array_t arr = patchvm_get (vm, instr->args[0].reg).arr;
    double alpha = instr->args[1].dbl;
    double beta = instr->args[2].dbl;
    array_lock (arr);
    array_ghw (arr, alpha, beta);
    array_unlock (arr);
}

// }}}

// bw {{{

void
array_bw (array_t arr, double a0, double a1, double a2)
{
    size_t i, len = arr->size;
    double x, dx = 2 * M_PI / (len - 1.0);
    samp_t* xs = arr->xs;

    for (i = 0, x = -M_PI; i < len; i++, x += dx)
        xs[i] *= a0 + a1 * cos (x) + a2 * cos (2 * x);
}

void
PATCHVM_array_bw (patchvm_t vm, instr_t instr)
{
    array_t arr = patchvm_get (vm, instr->args[0].reg).arr;
    double a0 = instr->args[1].dbl;
    double a1 = instr->args[2].dbl;
    double a2 = instr->args[3].dbl;
    array_lock (arr);
    array_bw (arr, a0, a1, a2);
    array_unlock (arr);
}

// }}}

// pcw {{{

void
array_pcw (array_t arr, double e)
{
    size_t i, len = arr->size;
    double x, dx = 2 * M_PI / (len - 1.0);
    samp_t* xs = arr->xs;

    for (i = 0, x = -M_PI; i < len; i++, x += dx)
        xs[i] *= pow (cos (x), e);
}

void
PATCHVM_array_pcw (patchvm_t vm, instr_t instr)
{
    array_t arr = patchvm_get (vm, instr->args[0].reg).arr;
    double e = instr->args[1].dbl;
    array_lock (arr);
    array_pcw (arr, e);
    array_unlock (arr);
}

// }}}

// load {{{

int
array_load (array_t* as, size_t nchan, const char* fn)
{
    SF_INFO sfi;
    sfi.format = 0;

    SNDFILE* sf = sf_open (fn, SFM_READ, &sfi);
    if (!sf)
    {
        log_emit (LOG_ERROR, "cannot opet %s for reading: %s", fn, sf_strerror (NULL));
        return -1;
    }

    if (sfi.channels != (int) nchan)
    {
        log_emit (LOG_ERROR, "%s has %d channels, expected %zu", fn, sfi.channels, nchan);
        sf_close (sf);
        return -1;
    }

    size_t len = sfi.frames;
    size_t i, j;

    for (i = 0; i < nchan; i++)
        as[i] = array_create (len);

    for (j = 0; j < len; j++)
    {
        samp_t buf[nchan];
        sf_readf_float (sf, buf, 1);
        for (i = 0; i < nchan; i++)
            as[i]->xs[j] = buf[i];
    }

    sf_close (sf);

    return 0;
}

void
PATCHVM_array_load1 (patchvm_t vm, instr_t instr)
{
    const char* fn = (char*) instr->args[0].utf8;
    array_t as[1];
    if (array_load (as, 1, fn) != 0)
        patchvm_fail (vm);
    else
        patchvm_set (vm, instr->args[1].reg, (reg_t) { .tag = T_ARRAY, .arr = as[0] });
}

void
PATCHVM_array_load2 (patchvm_t vm, instr_t instr)
{
    const char* fn = (char*) instr->args[0].utf8;
    array_t as[2];
    if (array_load (as, 2, fn) != 0)
        patchvm_fail (vm);
    else
    {
        patchvm_set (vm, instr->args[1].reg, (reg_t) { .tag = T_ARRAY, .arr = as[0] });
        patchvm_set (vm, instr->args[2].reg, (reg_t) { .tag = T_ARRAY, .arr = as[1] });
    }
}

// }}}

// lookup {{{

void
array_lookup (array_t arr, const samp_t* xs, samp_t* ys, size_t len)
{
    size_t i, n = arr->size;
    samp_t* as = arr->xs;

    for (i = 0; i < len; i++)
    {
        samp_t x = (1 + xs[i] - floor (xs[i])) * n;
        size_t k = floor (x);
        samp_t p = x - floor (x);

        size_t x_0 = (k - 1) % n;
        size_t x_1 = (k + 0) % n;
        size_t x_2 = (k + 1) % n;
        size_t x_3 = (k + 2) % n;

        samp_t c0 = as[x_1];
        samp_t c1 = 0.5 * (as[x_2] - as[x_0]);
        samp_t c2 = as[x_0] - 2.5 * as[x_1] + 2 * as[x_2] - 0.5 * as[x_3];
        samp_t c3 = 0.5 * (as[x_3] - as[x_0]) + 1.5 * (as[x_1] - as[x_2]);

        ys[i] = ((c3 * p + c2) * p + c1) * p + c0;
    }
}

// }}}

// vim:fdm=marker:
