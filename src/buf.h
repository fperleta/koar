
/*
 * koar/buf.h
 * copyright (c) 2013 Frano Perleta
 */

#ifndef KOAR_BUF_H
#define KOAR_BUF_H

#include <tgmath.h>
#include <pthread.h>
#include "defs.h"

// buffer size
#define BUF_SAMPLES_LOG 6u
#define BUF_SAMPLES (1u << BUF_SAMPLES_LOG)
#define BUF_BYTES_LOG (BUF_SAMPLES_LOG + 2)
#define BUF_BYTES (1u << BUF_BYTES_LOG)
#define BUF_BITS_LOG (BUF_BYTES_LOG + 3)
#define BUF_BITS (1u << BUF_BITS_LOG)

// buffer pool size
#define BUFPOOL_HEAD_LOG 2u // log2 of number of reserved bufs at the beggining of pool
#define BUFPOOL_HEAD (1u << BUFPOOL_HEAD_LOG)
#define BUFPOOL_TOTAL_LOG (BUF_BYTES_LOG + BUFPOOL_HEAD_LOG - 1u)
#define BUFPOOL_TOTAL (1u << BUFPOOL_TOTAL_LOG)
#define BUFPOOL_BYTES_LOG (BUFPOOL_TOTAL_LOG + BUF_BYTES_LOG)
#define BUFPOOL_BYTES (1u << BUFPOOL_BYTES_LOG)
#define BUFPOOL_MASK (BUFPOOL_BYTES - 1)

typedef struct bufpool_s* bufpool_t;

typedef float samp_t;
typedef samp_t samp4_t __attribute__((vector_size (16)));

typedef union {
    void* p;
    samp_t* xs;
    samp4_t* x4s;
} buf_t;

struct bufpool_s {
    uint8_t rc[BUFPOOL_TOTAL];
    pthread_mutex_t mutex;
    size_t free;
};

extern bufpool_t bufpool_create (void);
extern void bufpool_destroy (bufpool_t);

MACRO bufpool_t
bufpool_get (buf_t b)
{
    return (bufpool_t) (void*) (((uintptr_t) b.xs) & ~BUFPOOL_MASK);
}

MACRO size_t
bufpool_index (buf_t b)
{
    return (size_t) (((uintptr_t) b.xs) & BUFPOOL_MASK) >> BUF_BYTES_LOG;
}

extern buf_t buf_alloc (bufpool_t); // refcount = 1
extern void buf_acquire (buf_t);
extern void buf_release (buf_t);

MACRO void
buf_const (buf_t b, samp_t x, size_t n)
{
    size_t i;
    for (i = 0; i < n; i++)
        b.xs[i] = x;
}

MACRO samp_t
buf_lin (buf_t b, samp_t x0, samp_t dx, size_t n)
{
    samp4_t x4 = {x0, x0 + dx, x0 + 2*dx, x0 + 3*dx};
    //samp4_t dx4 = 4 * (samp4_t) dx;
    size_t i;
    for (i = 0; 4*i < n; i++, x4 += 4 * dx)
        b.x4s[i] = x4;
    return x0 + n * dx;
}

MACRO samp_t
buf_xdec (buf_t b, samp_t x0, samp_t xinf, samp_t tau, size_t n)
{
    samp_t f1 = exp (-tau);
    samp_t f4 = exp (-4*tau);
    samp_t h = x0 - xinf;
    samp4_t h4 = {1, f1, f1*f1, f1*f1*f1}; h4 *= h;
    size_t i;
    for (i = 0; 4*i < n; i++, h4 *= f4)
        b.x4s[i] = xinf + h4;
    return xinf + h * exp (-n * tau);
}

MACRO void
buf_add (buf_t acc, buf_t b, size_t n)
{
    size_t i;
    for (i = 0; 4*i < n; i++)
        acc.x4s[i] += b.x4s[i];
}

MACRO void
buf_mul (buf_t acc, buf_t b, size_t n)
{
    size_t i;
    for (i = 0; 4*i < n; i++)
        acc.x4s[i] *= b.x4s[i];
}

#endif /* koar/buf.h */

// vim:fdm=marker:
