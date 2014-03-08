
/*
 * koar/delay.c
 * copyright (c) 2014 Frano Perleta
 */

#include "delay.h"

// create {{{

delay_t
delay_create (size_t size, patch_stamp_t now)
{
    samp_t* xs = xmalloc (sizeof (samp_t) * size);
    delay_t del = xmalloc (sizeof (struct delay_s));

    int res = pthread_mutex_init (&(del->mutex), NULL);
    if (res)
        panic ("pthread_mutex_init() returned %d", res);

    del->refcount = 1;
    del->size = size;
    del->head = 0;
    del->stamp = now;
    del->xs = xs;

    size_t i;
    for (i = 0; i < size; i++)
        xs[i] = 0;

    return del;
}

// }}}

// destroy {{{

static void
delay_destroy (delay_t del)
{
    int res = pthread_mutex_destroy (&(del->mutex));
    if (res)
        panic ("pthread_mutex_destroy() returned %d", res);
    if (del->xs)
        free (del->xs);
    free (del);
}

// }}}

// acquire {{{

delay_t
delay_acquire (delay_t del)
{
    delay_lock (del);
    del->refcount++;
    delay_unlock (del);
    return del;
}

// }}}

// release {{{

void
delay_release (delay_t del)
{
    delay_lock (del);
    int last = !(--del->refcount);
    delay_unlock (del);
    if (last)
        delay_destroy (del);
}

// }}}

// push {{{

void
delay_push (delay_t del, patch_stamp_t now, size_t delta, const samp_t* buf)
{
    delay_lock (del);

    size_t new_head = del->head + delta;

    if (new_head > del->size)
    {
        size_t i, j;
        for (i = del->head, j = 0; i < del->size; i++, j++)
            del->xs[i] = buf[j];
        for (i = 0; j < delta; i++, j++)
            del->xs[i] = buf[j];
        del->head = new_head - del->size;
    }
    else
    {
        size_t i, j;
        for (i = del->head, j = 0; j < delta; i++, j++)
            del->xs[i] = buf[j];
        del->head = new_head;
    }

    del->stamp = now + delta;

    delay_unlock (del);
}

// }}}

// qtap {{{

void
delay_qtap (delay_t del, patch_stamp_t when, size_t len, buf_t b)
{
    delay_lock (del);

    size_t offs = del->stamp - when;

    if ((offs > del->size) || (offs < len))
    {
        buf_const (b, 0, len);
        goto done;
    }

    if (offs <= del->head) // [ #### >   ]
    {
        size_t i, j;
        for (i = del->head - offs, j = 0; j < len; i++, j++)
            b.xs[j] = del->xs[i];
    }
    else if (offs - len > del->head) // [   > #### ]
    {
        size_t i, j;
        for (i = del->head + del->size - offs, j = 0; j < len; i++, j++)
            b.xs[j] = del->xs[i];
    }
    else // [##  >  ##]
    {
        size_t i, j;
        for (i = del->head + del->size - offs, j = 0; i < del->size; i++, j++)
            b.xs[j] = del->xs[i];
        for (i = 0; j < len; i++, j++)
            b.xs[j] = del->xs[i];
    }

done:
    delay_unlock (del);
}

// }}}

// vim:fdm=marker:
