
/*
 * koar/buf.c
 * copyright (c) 2013 Frano Perleta
 */

#include "buf.h"

bufpool_t
bufpool_create (void)
{
    bufpool_t pool = NULL;

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

    log_emit (LOG_DETAIL,
        "bufpool: ptr=%p, head=%zu, free=%zu, size=%zukB",
        pool,
        BUFPOOL_HEAD,
        pool->free,
        BUFPOOL_BYTES >> 10u);

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
#if DEBUG_BUFS
buf_alloc_ (const char* fn, int ln, bufpool_t pool)
#else
buf_alloc (bufpool_t pool)
#endif
{
    pthread_mutex_lock (&(pool->mutex));

    if (!pool->free)
        panic ("bufpool overflow");

    size_t i;
    for (i = BUFPOOL_HEAD; (i < BUFPOOL_TOTAL) && pool->rc[i]; i++);
    if (i >= BUFPOOL_TOTAL)
        panic ("bufpool overflow");
    pool->rc[i] = 1;
    pool->free--;

#if DEBUG_BUFS
    log_emit (LOG_DEBUG, "buf_alloc (%zu) @ %s:%d", i, fn, ln);
#endif

    pthread_mutex_unlock (&(pool->mutex));
    return (buf_t) (((void*) pool) + (i << BUF_BYTES_LOG));
}

buf_t
#if DEBUG_BUFS
buf_acquire_ (const char* fn, int ln, buf_t b)
#else
buf_acquire (buf_t b)
#endif
{
    bufpool_t pool = bufpool_get (b);
    size_t i = bufpool_index (b);

    if ((i < BUFPOOL_HEAD) && (i >= BUFPOOL_TOTAL))
        panic ("invalid bufpool index %zu (ptr = %p)", i, b.p);

#if DEBUG_BUFS
    log_emit (LOG_DEBUG, "buf_acquire (%zu)", i, fn, ln);
#endif

    pthread_mutex_lock (&(pool->mutex));
    if (pool->rc[i] == 0xFF)
        panic ("buffer reference overflow (ptr = %p, index = %zu)", b.p, i);
    pool->rc[i]++;
    pthread_mutex_unlock (&(pool->mutex));

    return b;
}

void
#if DEBUG_BUFS
buf_release_ (const char* fn, int ln, buf_t b)
#else
buf_release (buf_t b)
#endif
{
    bufpool_t pool = bufpool_get (b);
    size_t i = bufpool_index (b);

    if ((i < BUFPOOL_HEAD) && (i >= BUFPOOL_TOTAL))
        panic ("invalid bufpool index %zu (ptr = %p)", i, b.p);

    pthread_mutex_lock (&(pool->mutex));
    if (!pool->rc[i])
        panic ("buffer reference underflow");
    pool->rc[i]--;
    if (!pool->rc[i])
    {
#if DEBUG_BUFS
        log_emit (LOG_DEBUG, "buf_free (%zu) @ %s:%d", i, fn, ln);
#endif
        pool->free++;
    }
#if DEBUG_BUFS
    else log_emit (LOG_DEBUG, "buf_release (%zu) @ %s:%d", i, fn, ln);
#endif
    pthread_mutex_unlock (&(pool->mutex));
}

// vim:fdm=marker:
