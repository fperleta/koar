
/*
 * koar/defs.h
 * copyright (c) 2013 Frano Perleta
 */

#ifndef KOAR_DEFS_H
#define KOAR_DEFS_H

#define KOAR_VERSION "0"

// includes {{{
#include <stdint.h>
#include <stdlib.h>
// }}}

#define PACKED __attribute__((packed))
#define UNUSED __attribute__((unused))
#define MACRO static inline __attribute__((always_inline, unused))

#define likely(x) (__builtin_expect (!!(x), 1))
#define unlikely(x) (__builtin_expect (!!(x), 0))

typedef enum {
    LOG_ERROR = 0,
    LOG_NORMAL,
    LOG_DETAIL,
    LOG_DEBUG
} log_level_t;

extern void __log_emit (const char*, int, log_level_t, const char*, ...);
#define log_emit(lvl, fmt, ...) __log_emit (__FILE__, __LINE__, lvl, fmt, ## __VA_ARGS__)

#ifdef DEBUG
#define DEBUGF(fmt, ...) __log_emit (__FILE__, __LINE__, LOG_DEBUG, "%s: " fmt, __func__, ## __VA_ARGS__)
#define DEBUGP(fmt, expr) __log_emit (__FILE__, __LINE__, LOG_DEBUG, "%s: " #expr " = " fmt, __func__, expr)
#else
#define DEBUGF(fmt, ...)
#define DEBUGP(fmt, expr)
#endif

extern void __panic (void) __attribute__((noreturn));
#define panic(fmt, ...) do { log_emit (LOG_ERROR, fmt, ## __VA_ARGS__); __panic (); } while (0)

extern void* xmalloc (size_t);
extern void* xrealloc (void*, size_t);
#define xmalloc_union(s,f,t) xalloc (__builtin_offsetof (s, f) + sizeof (t))

typedef union {
    void* p;
    intptr_t i;
    uintptr_t n;
    double d;
} word_t;

#endif /* koar/defs.h */

