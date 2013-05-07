
/*
 * koar/buf.c
 * copyright (c) 2013 Frano Perleta
 */

#include "buf.h"

bufpool_t
bufpool_create (void)
{
    bufpool_t pool;

    int res = posix_memalign ((void**) &pool, BUFPOOL_BYTES, BUFPOOL_BYTES);
    if (res)
        panic ("posix_memalign: %m");

    size_t i;
    for (i = 0; i < BUFPOOL_HEAD; i++)
        pool->rc[i] = 0xFF;
    for (; i < BUFPOOL_TOTAL; i++)
        pool->rc[i] = 0;

    pool->free = BUFPOOL_TOTAL - BUFPOOL_HEAD;

    res = pthread_mutex_init (&(pool->mutex), NULL);
    if (res)
        panic ("pthread_mutex_init() returned %d", res);

    return pool;
}

void
bufpool_destroy (bufpool_t pool)
{
    int res = pthread_mutex_destroy (&(pool->mutex));
    if (res)
        panic ("pthread_mutex_destroy() returned %d", res);
    free (pool);
}

buf_t
buf_alloc (bufpool_t pool)
{
    pthread_mutex_lock (&(pool->mutex));

    if (!pool->free)
        panic ("bufpool overflow");

    size_t i;
    for (i = BUFPOOL_HEAD; (i < BUFPOOL_TOTAL) && pool->rc[i]; i++)
    pool->rc[i] = 1;
    pool->free--;

    pthread_mutex_unlock (&(pool->mutex));
    return (buf_t) (((void*) pool) + (i << BUF_BYTES_LOG));
}

void
buf_acquire (buf_t b)
{
    bufpool_t pool = bufpool_get (b);
    size_t i = bufpool_index (b);

    pthread_mutex_lock (&(pool->mutex));
    if (pool->rc[i] == 0xFF)
        panic ("buffer reference overflow");
    pool->rc[i]++;
    pthread_mutex_unlock (&(pool->mutex));
}

void
buf_release (buf_t b)
{
    bufpool_t pool = bufpool_get (b);
    size_t i = bufpool_index (b);

    pthread_mutex_lock (&(pool->mutex));
    if (!pool->rc[i])
        panic ("buffer reference underflow");
    pool->rc[i]--;
    if (!pool->rc[i])
        pool->free++;
    pthread_mutex_unlock (&(pool->mutex));
}

// vim:fdm=marker:
