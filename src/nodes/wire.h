
/*
 * koar/nodes/wire.h
 * copyright (c) 2014 Frano Perleta
 */

#ifndef KOAR_NODES_WIRE_H
#define KOAR_NODES_WIRE_H

#include "patch.h"
#include "defs.h"

extern anode_t N_wire_make (patch_t, pnode_t, pnode_t, double);
extern void N_wire_scale (anode_t, double);

#endif /* koar/nodes/wire.h */

// vim:fdm=marker:
