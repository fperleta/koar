
/*
 * koar/proto.h
 * copyright (c) 2013 Frano Perleta
 */

#ifndef KOAR_PROTO_H
#define KOAR_PROTO_H

#include "defs.h"

typedef uint32_t proto_mid_t;

typedef struct {
    union {
        struct {
            uint32_t mid:31;
            unsigned reply:1;
        } PACKED;
        uint32_t midr;
    } PACKED;
    uint32_t bytes;
} PACKED proto_hdr_t;

typedef struct {
    proto_hdr_t header;
    uint8_t payload[];
} proto_msg_t;

static inline proto_msg_t*
proto_msg_alloc (size_t size)
{
    proto_msg_t* msg = xmalloc (sizeof (proto_hdr_t) + size);
    msg->header.bytes = size;
    return msg;
}

#endif /* koar/proto.h */

// vim:fdm=marker:
