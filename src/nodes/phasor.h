
/*
 * koar/nodes/phasor.h
 * copyright (c) 2014 Frano Perleta
 */

#ifndef KOAR_NODES_PHASOR_H
#define KOAR_NODES_PHASOR_H

#include "patch.h"
#include "defs.h"

extern anode_t N_phasor_make (patch_t, pnode_t, pnode_t);
extern void N_phasor_jump (anode_t, samp_t);

#endif /* koar/nodes/phasor.h */

// vim:fdm=marker:
