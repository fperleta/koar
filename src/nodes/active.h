
/*
 * koar/nodes/active.h
 * copyright (c) 2014 Frano Perleta
 */

#ifndef KOAR_NODES_ACTIVE_H
#define KOAR_NODES_ACTIVE_H

#include "array.h"
#include "buf.h"
#include "patch.h"
#include "defs.h"

// cos2pis {{{

extern anode_t N_cos2pi_make (patch_t, pnode_t, pnode_t);

// }}}

// envs {{{

extern anode_t N_env_make (patch_t, pnode_t, samp_t);
extern void N_env_const (anode_t, samp_t);
extern void N_env_lin (anode_t, samp_t, double);
extern void N_env_xdec (anode_t, samp_t, double);

// }}}

// fwriters {{{

extern anode_t N_fwriter1_make (patch_t, const char*, pnode_t);
extern anode_t N_fwriter2_make (patch_t, const char*, pnode_t, pnode_t);
extern void N_fwriter_close (patch_t, anode_t);

// }}}

// lookups {{{

extern anode_t N_lookup_make (patch_t, array_t, pnode_t, pnode_t);
extern void N_lookup_table (anode_t, array_t);

// }}}

// phasors {{{

extern anode_t N_phasor_make (patch_t, pnode_t, pnode_t);
extern void N_phasor_jump (anode_t, samp_t);

// }}}

// touches {{{

extern anode_t N_touch (patch_t, pnode_t);

// }}}

// wires {{{

extern anode_t N_wire_make (patch_t, pnode_t, pnode_t, double);
extern void N_wire_scale (anode_t, double);

// }}}

#endif /* koar/nodes/active.h */

// vim:fdm=marker:
