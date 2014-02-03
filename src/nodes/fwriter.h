
/*
 * koar/nodes/fwriter.h
 * copyright (c) 2014 Frano Perleta
 */

#ifndef KOAR_NODES_FWRITER_H
#define KOAR_NODES_FWRITER_H

#include "defs.h"

extern anode_t N_fwriter1_make (patch_t, const char*, pnode_t);
extern anode_t N_fwriter2_make (patch_t, const char*, pnode_t, pnode_t);
extern void N_fwriter_close (patch_t, anode_t);

#endif /* koar/nodes/fwriter.h */

// vim:fdm=marker:
