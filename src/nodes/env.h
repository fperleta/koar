
/*
 * koar/nodes/env.h
 * copyright (c) 2014 Frano Perleta
 */

#ifndef KOAR_NODES_ENV_H
#define KOAR_NODES_ENV_H

#include "buf.h"
#include "patch.h"
#include "defs.h"

extern anode_t N_env_make (patch_t, pnode_t, samp_t);
extern void N_env_const (anode_t, samp_t);
extern void N_env_lin (anode_t, samp_t, double);
extern void N_env_xdec (anode_t, samp_t, double);

#endif /* koar/nodes/env.h */

// vim:fdm=marker:
