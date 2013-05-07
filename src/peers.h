
/*
 * koar/peers.h
 * copyright (c) 2013 Frano Perleta
 */

#ifndef KOAR_PEERS_H
#define KOAR_PEERS_H

#include <ev.h>
#include "proto.h"
#include "defs.h"

// peers {{{

typedef struct peer_s* peer_t;
typedef struct peer_beh_s* peer_beh_t;
typedef struct peer_cont_s* peer_cont_t;

struct peer_beh_s {
    void (*on_message) (peer_t, proto_msg_t);
    void (*on_shutdown) (peer_t);
};

struct peer_cont_s {
    void (*on_reply) (peer_t, void*, proto_msg_t);
    void (*on_noreply) (peer_t, void*);
};

extern peer_t peer_connect (struct ev_loop*, peer_beh_t, const char*);

extern void peer_send (peer_t, proto_msg_t);
extern void peer_request (peer_t, proto_msg_t, peer_cont_t, void*);
extern void peer_reply (peer_t, proto_mid_t, proto_msg_t);

// }}}

// listeners {{{

typedef struct listener_s* listener_t;

extern listener_t listener_create (struct ev_loop*, peer_beh_t, const char*);
extern void listener_destroy (listener_t);

// }}}

#endif /* koar/peers.h */

// vim:fdm=marker:
