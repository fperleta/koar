
/*
 * koar/patchctl.h
 * copyright (c) 2013 Frano Perleta
 */

#ifndef KOAR_PATCHCTL_H
#define KOAR_PATCHCTL_H

#include <ev.h>
#include "patch.h"
#include "peers.h"
#include "defs.h"

// endpoints {{{

typedef struct patchctl_endpoint_s* patchctl_endpoint_t;

extern patchctl_endpoint_t patchctl_endpoint_create (struct ev_loop*, const char*, size_t, size_t);
extern void patchctl_endpoint_destroy (patchctl_endpoint_t);

// }}}

// patchctls {{{

typedef struct patchctl_s* patchctl_t;

typedef enum {
    PATCHCTL_FREE = 0,
    PATCHCTL_BOUND,
    PATCHCTL_ERROR
} patchctl_state_t;

//extern patchctl_t patchctl_create (const char*, size_t);
//extern void patchctl_destroy (patchctl_t);

// }}}

#endif /* koar/patchctl.h */

// vim:fdm=marker:
