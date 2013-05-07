
/*
 * koar/peers.h
 * copyright (c) 2013 Frano Perleta
 */

#ifndef KOAR_PEERS_H
#define KOAR_PEERS_H

#include "proto.h"
#include "defs.h"

// peers {{{

typedef struct peer_t peer_t;

typedef struct {
    void (*on_message) (peer_t*, proto_msg_t*);
    void (*on_shutdown) (peer_t*);
} peer_beh_t;

typedef struct {
    void (*on_reply) (peer_t*, void*, proto_msg_t*);
    void (*on_noreply) (peer_t*, void*);
} peer_cont_t;

extern peer_t* peer_connect (peer_beh_t*, const char*);

extern void peer_send (peer_t*, proto_msg_t*);
extern void peer_request (peer_t*, proto_msg_t*, peer_cont_t*, void*);
extern void peer_reply (peer_t*, proto_mid_t, proto_msg_t*);

// }}}

// listeners {{{

typedef struct listener_t listener_t;

extern listener_t* listener_create (peer_beh_t*, const char*);
extern void listener_destroy (listener_t*);

// }}}

#endif /* koar/peers.h */

// vim:fdm=marker: