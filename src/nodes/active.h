
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

// biquads {{{

extern anode_t N_biquad_make (patch_t, pnode_t, pnode_t, size_t);
extern void N_biquad_gain (anode_t, samp_t);
extern void N_biquad_coeffs (anode_t, size_t, samp_t, samp_t, samp_t, samp_t);

// }}}

// blits {{{

extern anode_t N_blit_make (patch_t, pnode_t, pnode_t);
extern void N_blit_gain (anode_t, samp_t);
extern void N_blit_jump (anode_t, samp_t);
extern void N_blit_unipolar (anode_t);
extern void N_blit_bipolar (anode_t);

// }}}

// cos2pis {{{

extern anode_t N_cos2pi_make (patch_t, pnode_t, pnode_t);

// }}}

// dtaps {{{

extern anode_t N_dtap_make (patch_t, pnode_t, anode_t, size_t);
extern void N_dtap_adjust (anode_t, size_t);

// }}}

// dwriters {{{

extern anode_t N_dwriter_make (patch_t, pnode_t, size_t);

// }}}

// envs {{{

extern anode_t N_env_make (patch_t, pnode_t, samp_t);
extern void N_env_const (anode_t, samp_t);
extern void N_env_lin (anode_t, samp_t, double);
extern void N_env_xdec (anode_t, samp_t, double);

// }}}

// fwriters {{{

extern anode_t N_fwriter1_make (patch_t, const char*, size_t, pnode_t);
extern anode_t N_fwriter2_make (patch_t, const char*, size_t, pnode_t, pnode_t);
extern void N_fwriter_close (patch_t, anode_t);

// }}}

// lookups {{{

extern anode_t N_lookup_make (patch_t, array_t, pnode_t, pnode_t);
extern void N_lookup_table (anode_t, array_t);

// }}}

// moogs {{{

extern anode_t N_moog_make (patch_t, pnode_t, pnode_t, pnode_t, pnode_t);

// }}}

// noises {{{

extern anode_t N_noise_make (patch_t, pnode_t, unsigned);
extern void N_noise_seed (anode_t, unsigned);
extern void N_noise_white (anode_t);
extern void N_noise_pink (anode_t);

// }}}

// phasors {{{

extern anode_t N_phasor_make (patch_t, pnode_t, pnode_t);
extern void N_phasor_jump (anode_t, samp_t);

// }}}

// resons {{{

extern anode_t N_reson_make (patch_t, pnode_t, pnode_t, pnode_t, pnode_t);
extern void N_reson_pure (anode_t, samp_t);
extern void N_reson_res (anode_t, samp_t);
extern void N_reson_peak (anode_t, samp_t);
extern void N_reson_lowpass (anode_t, samp_t);

// }}}

// reverbs {{{

extern anode_t N_reverb_make (patch_t, pnode_t, pnode_t, pnode_t, pnode_t, size_t, size_t, size_t);
extern void N_reverb_internal (anode_t, size_t, size_t, size_t, samp_t, samp_t);
extern void N_reverb_sources (anode_t, size_t, size_t, samp_t, samp_t, size_t, samp_t, samp_t);
extern void N_reverb_sinks (anode_t, size_t, size_t, samp_t, samp_t, size_t, samp_t, samp_t);
extern void N_reverb_tcfilter (anode_t, samp_t);

// }}}

// tanhs {{{

extern anode_t N_tanh_make (patch_t, pnode_t, pnode_t);
extern void N_tanh_gain (anode_t, samp_t);
extern void N_tanh_slope (anode_t, samp_t);

// }}}

// touches {{{

extern anode_t N_touch (patch_t, pnode_t);

// }}}

// vdelays {{{

extern anode_t N_vdelay_make (patch_t, pnode_t, pnode_t, pnode_t, size_t);
extern void N_vdelay_gains (anode_t, samp_t, samp_t, samp_t);

// }}}

// wires {{{

extern anode_t N_wire_make (patch_t, pnode_t, pnode_t, double);
extern void N_wire_scale (anode_t, double);

// }}}

#endif /* koar/nodes/active.h */

// vim:fdm=marker:
