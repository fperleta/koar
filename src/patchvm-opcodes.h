
/*
 * koar/patchvm-opcodes.h
 * copyright (c) 2014 Frano Perleta
 */

#define RESERVED(n) \
    OPCODE (n, "reserved", nop) END

// (0 - 15) basic instructions {{{

OPCODE (0, "nop", nop)
END

OPCODE (1, "leave", leave)
END

OPCODE (2, "resize", resize)
    ARG (A_NAT, "nregs")
END

OPCODE (3, "blank", blank)
    ARG (A_ANYREG, "ref")
END

OPCODE (4, "move", move)
    ARG (A_ANYREG, "src")
    ARG (A_ANYREG, "dst")
END

OPCODE (5, "dup", dup)
    ARG (A_ANYREG, "src")
    ARG (A_ANYREG, "dst")
END

OPCODE (6, "advance", advance)
    ARG (A_NAT, "periods")
END

// {{{
RESERVED (7)
RESERVED (8)
RESERVED (9)
RESERVED (10)
RESERVED (11)
RESERVED (12)
RESERVED (13)
RESERVED (14)
RESERVED (15)
// }}}

// }}}

// (16 - 63) arrays {{{

OPCODE (16, "array/make", array_make)
    ARG (A_ANYREG, "self")
    ARG (A_NAT, "size")
    ARG (A_DOUBLE, "x0")
END

OPCODE (17, "array/const", array_const)
    ARG (A_REG + T_ARRAY, "self")
    ARG (A_DOUBLE, "x0")
END

OPCODE (18, "array/normalize", array_normalize)
    ARG (A_REG + T_ARRAY, "self")
    ARG (A_DOUBLE, "amp")
END

OPCODE (19, "array/dc", array_dc)
    ARG (A_REG + T_ARRAY, "self")
    ARG (A_DOUBLE, "offset")
END

OPCODE (20, "array/partial", array_partial)
    ARG (A_REG + T_ARRAY, "self")
    ARG (A_DOUBLE, "amp")
    ARG (A_NAT, "index")
    ARG (A_DOUBLE, "phase")
END

OPCODE (21, "array/ghw", array_ghw)
    ARG (A_REG + T_ARRAY, "self")
    ARG (A_DOUBLE, "alpha")
    ARG (A_DOUBLE, "beta")
END

OPCODE (22, "array/bw", array_bw)
    ARG (A_REG + T_ARRAY, "self")
    ARG (A_DOUBLE, "a0")
    ARG (A_DOUBLE, "a1")
    ARG (A_DOUBLE, "a2")
END

OPCODE (23, "array/pcw", array_pcw)
    ARG (A_REG + T_ARRAY, "self")
    ARG (A_DOUBLE, "e")
END

OPCODE (24, "array/load1", array_load1)
    ARG (A_UTF8, "path")
    ARG (A_ANYREG, "out")
END

OPCODE (25, "array/load2", array_load2)
    ARG (A_UTF8, "path")
    ARG (A_ANYREG, "left")
    ARG (A_ANYREG, "right")
END

// {{{
RESERVED (26)
RESERVED (27)
RESERVED (28)
RESERVED (29)
RESERVED (30)
RESERVED (31)
RESERVED (32)
RESERVED (33)
RESERVED (34)
RESERVED (35)
RESERVED (36)
RESERVED (37)
RESERVED (38)
RESERVED (39)
RESERVED (40)
RESERVED (41)
RESERVED (42)
RESERVED (43)
RESERVED (44)
RESERVED (45)
RESERVED (46)
RESERVED (47)
RESERVED (48)
RESERVED (49)
RESERVED (50)
RESERVED (51)
RESERVED (52)
RESERVED (53)
RESERVED (54)
RESERVED (55)
RESERVED (56)
RESERVED (57)
RESERVED (58)
RESERVED (59)
RESERVED (60)
RESERVED (61)
RESERVED (62)
RESERVED (63)
// }}}

// }}}

// (64 - 71) passive nodes {{{

OPCODE (64, "sum", sum)
    ARG (A_ANYREG, "self")
END

OPCODE (65, "prod", prod)
    ARG (A_ANYREG, "self")
END

// {{{
RESERVED (66)
RESERVED (67)
RESERVED (68)
RESERVED (69)
RESERVED (70)
RESERVED (71)
// }}}

// }}}

// (72 - 73) touches {{{

OPCODE (72, "touch", touch)
    ARG (A_ANYREG, "self")
    ARG (A_REG + T_PNODE, "snk")
END

OPCODE (73, "reserved", nop)
END

// }}}

// (74 - 75) wires {{{

OPCODE (74, "wire/make", wire_make)
    ARG (A_ANYREG, "self")
    ARG (A_REG + T_PNODE, "src")
    ARG (A_REG + T_PNODE, "snk")
    ARG (A_DOUBLE, "scale")
END

OPCODE (75, "wire/scale", wire_scale)
    ARG (A_REG + T_WIRE, "self")
    ARG (A_DOUBLE, "scale")
END

// }}}

// (76 - 79) fwriters {{{

OPCODE (76, "fwriter1/make", fwriter1_make)
    ARG (A_ANYREG, "self")
    ARG (A_UTF8, "filepath")
    ARG (A_NAT, "samplerate")
    ARG (A_REG + T_PNODE, "src")
END

OPCODE (77, "fwriter1/close", fwriter_close)
    ARG (A_REG + T_FWRITER1, "self")
END

OPCODE (78, "fwriter2/make", fwriter2_make)
    ARG (A_ANYREG, "self")
    ARG (A_UTF8, "filepath")
    ARG (A_NAT, "samplerate")
    ARG (A_REG + T_PNODE, "left")
    ARG (A_REG + T_PNODE, "right")
END

OPCODE (79, "fwriter2/close", fwriter_close)
    ARG (A_REG + T_FWRITER2, "self")
END

// }}}

// (80 - 89) envs {{{

OPCODE (80, "env/make", env_make)
    ARG (A_ANYREG, "self")
    ARG (A_REG + T_PNODE, "snk")
    ARG (A_DOUBLE, "x0")
END

OPCODE (81, "env/const", env_const)
    ARG (A_REG + T_ENV, "self")
    ARG (A_DOUBLE, "x0")
END

OPCODE (82, "env/lin", env_lin)
    ARG (A_REG + T_ENV, "self")
    ARG (A_DOUBLE, "x1")
    ARG (A_DOUBLE, "t")
END

OPCODE (83, "env/xdec", env_xdec)
    ARG (A_REG + T_ENV, "self")
    ARG (A_DOUBLE, "xinf")
    ARG (A_DOUBLE, "tau")
END

OPCODE (84, "env/cos", env_cos)
    ARG (A_REG + T_ENV, "self")
    ARG (A_DOUBLE, "x1")
    ARG (A_DOUBLE, "t")
END

// {{{
RESERVED (85)
RESERVED (86)
RESERVED (87)
RESERVED (88)
RESERVED (89)
// }}}

// }}}

// (90 - 91) phasors {{{

OPCODE (90, "phasor/make", phasor_make)
    ARG (A_ANYREG, "self")
    ARG (A_REG + T_PNODE, "src")
    ARG (A_REG + T_PNODE, "snk")
END

OPCODE (91, "phasor/jump", phasor_jump)
    ARG (A_REG + T_PHASOR, "self")
    ARG (A_DOUBLE, "phase")
END

// }}}

// (92 - 93) cos2pis {{{

OPCODE (92, "cos2pi/make", cos2pi_make)
    ARG (A_ANYREG, "self")
    ARG (A_REG + T_PNODE, "src")
    ARG (A_REG + T_PNODE, "snk")
END

RESERVED (93)

// }}}

// (94 - 95) lookups {{{

OPCODE (94, "lookup/make", lookup_make)
    ARG (A_ANYREG, "self")
    ARG (A_REG + T_ARRAY, "table")
    ARG (A_REG + T_PNODE, "src")
    ARG (A_REG + T_PNODE, "snk")
END

OPCODE (95, "lookup/table", lookup_table)
    ARG (A_REG + T_LOOKUP, "self")
    ARG (A_REG + T_ARRAY, "table")
END

// }}}

// (96 - 99) noises {{{

OPCODE (96, "noise/make", noise_make)
    ARG (A_ANYREG, "self")
    ARG (A_REG + T_PNODE, "snk")
    ARG (A_NAT, "seed")
END

OPCODE (97, "noise/seed", noise_seed)
    ARG (A_REG + T_NOISE, "self")
    ARG (A_NAT, "seed")
END

OPCODE (98, "noise/white", noise_white)
    ARG (A_REG + T_NOISE, "self")
END

OPCODE (99, "noise/pink", noise_pink)
    ARG (A_REG + T_NOISE, "self")
END

// }}}

// (100 - 101) dwriters {{{

OPCODE (100, "dwriter/make", dwriter_make)
    ARG (A_ANYREG, "self")
    ARG (A_REG + T_PNODE, "src")
    ARG (A_NAT, "size")
END

RESERVED (101)

// }}}

// (102 - 103) dtaps {{{

OPCODE (102, "dtap/make", dtap_make)
    ARG (A_ANYREG, "self")
    ARG (A_REG + T_PNODE, "snk")
    ARG (A_REG + T_DWRITER, "from")
    ARG (A_NAT, "offs")
END

OPCODE (103, "dtap/adjust", dtap_adjust)
    ARG (A_REG + T_DTAP, "self")
    ARG (A_NAT, "offs")
END

// }}}

// (104 - 107) vdelays {{{

OPCODE (104, "vdelay/make", vdelay_make)
    ARG (A_ANYREG, "self")
    ARG (A_REG + T_PNODE, "src")
    ARG (A_REG + T_PNODE, "dsig")
    ARG (A_REG + T_PNODE, "snk")
    ARG (A_NAT, "len")
END

OPCODE (105, "vdelay/gains", vdelay_gains)
    ARG (A_REG + T_VDELAY, "self")
    ARG (A_DOUBLE, "raw")
    ARG (A_DOUBLE, "del")
    ARG (A_DOUBLE, "fb")
END

OPCODE (106, "vdelay/freq-mode", vdelay_freqmode)
    ARG (A_REG + T_VDELAY, "self")
END

RESERVED (107)

// }}}

// (108 - 111) biquads {{{

OPCODE (108, "biquad/make", biquad_make)
    ARG (A_ANYREG, "self")
    ARG (A_REG + T_PNODE, "src")
    ARG (A_REG + T_PNODE, "snk")
    ARG (A_NAT, "stages")
END

OPCODE (109, "biquad/gain", biquad_gain)
    ARG (A_REG + T_BIQUAD, "self")
    ARG (A_DOUBLE, "gain")
END

OPCODE (110, "biquad/coeffs", biquad_coeffs)
    ARG (A_REG + T_BIQUAD, "self")
    ARG (A_NAT, "stage")
    ARG (A_DOUBLE, "b1")
    ARG (A_DOUBLE, "b2")
    ARG (A_DOUBLE, "a1")
    ARG (A_DOUBLE, "a2")
END

RESERVED (111)

// }}}

// (112 - 117) blits {{{

OPCODE (112, "blit/make", blit_make)
    ARG (A_ANYREG, "self")
    ARG (A_REG + T_PNODE, "freq")
    ARG (A_REG + T_PNODE, "snk")
END

OPCODE (113, "blit/gain", blit_gain)
    ARG (A_REG + T_BLIT, "self")
    ARG (A_DOUBLE, "gain")
END

OPCODE (114, "blit/jump", blit_jump)
    ARG (A_REG + T_BLIT, "self")
    ARG (A_DOUBLE, "phase")
END

OPCODE (115, "blit/unipolar", blit_unipolar)
    ARG (A_REG + T_BLIT, "self")
END

OPCODE (116, "blit/bipolar", blit_bipolar)
    ARG (A_REG + T_BLIT, "self")
END

RESERVED (117)

// }}}

// (118 - 123) resons {{{

OPCODE (118, "reson/make", reson_make)
    ARG (A_ANYREG, "self")
    ARG (A_REG + T_PNODE, "src")
    ARG (A_REG + T_PNODE, "fsig")
    ARG (A_REG + T_PNODE, "qsig")
    ARG (A_REG + T_PNODE, "snk")
END

OPCODE (119, "reson/pure", reson_pure)
    ARG (A_REG + T_RESON, "self")
    ARG (A_DOUBLE, "gain")
END

OPCODE (120, "reson/res", reson_res)
    ARG (A_REG + T_RESON, "self")
    ARG (A_DOUBLE, "gain")
END

OPCODE (121, "reson/peak", reson_peak)
    ARG (A_REG + T_RESON, "self")
    ARG (A_DOUBLE, "gain")
END

OPCODE (122, "reson/lowpass", reson_lowpass)
    ARG (A_REG + T_RESON, "self")
    ARG (A_DOUBLE, "gain")
END

OPCODE (123, "reson/highpass", reson_highpass)
    ARG (A_REG + T_RESON, "self")
    ARG (A_DOUBLE, "gain")
END

// }}}

// (124 - 127) tanhs {{{

OPCODE (124, "tanh/make", tanh_make)
    ARG (A_ANYREG, "self")
    ARG (A_REG + T_PNODE, "src")
    ARG (A_REG + T_PNODE, "snk")
END

OPCODE (125, "tanh/gain", tanh_gain)
    ARG (A_REG + T_TANH, "self")
    ARG (A_DOUBLE, "gain")
END

OPCODE (126, "tanh/slope", tanh_slope)
    ARG (A_REG + T_TANH, "self")
    ARG (A_DOUBLE, "gain")
END

RESERVED (127)

// }}}

// (128 - 129) moogs {{{

OPCODE (128, "moog/make", moog_make)
    ARG (A_ANYREG, "self")
    ARG (A_REG + T_PNODE, "src")
    ARG (A_REG + T_PNODE, "fsig")
    ARG (A_REG + T_PNODE, "ksig")
    ARG (A_REG + T_PNODE, "snk")
END

OPCODE (129, "moog/params", moog_params)
    ARG (A_REG + T_MOOG, "self")
    ARG (A_DOUBLE, "gain")
    ARG (A_DOUBLE, "drive")
    ARG (A_DOUBLE, "thermal")
END

// }}}

// (130 - 135) reverbs {{{

OPCODE (130, "reverb/make", reverb_make)
    ARG (A_ANYREG, "self")
    ARG (A_REG + T_PNODE, "src1")
    ARG (A_REG + T_PNODE, "src2")
    ARG (A_REG + T_PNODE, "snk1")
    ARG (A_REG + T_PNODE, "snk2")
    ARG (A_NAT, "nwalls")
    ARG (A_NAT, "wall_len")
    ARG (A_NAT, "sink_len")
END

OPCODE (131, "reverb/internal", reverb_internal)
    ARG (A_REG + T_REVERB, "self")
    ARG (A_NAT, "w1")
    ARG (A_NAT, "w2")
    ARG (A_NAT, "offs")
    ARG (A_DOUBLE, "g")
    ARG (A_DOUBLE, "p")
END

OPCODE (132, "reverb/sources", reverb_sources)
    ARG (A_REG + T_REVERB, "self")
    ARG (A_NAT, "w")
    ARG (A_NAT, "loffs")
    ARG (A_DOUBLE, "lg")
    ARG (A_DOUBLE, "lp")
    ARG (A_NAT, "roffs")
    ARG (A_DOUBLE, "rg")
    ARG (A_DOUBLE, "rp")
END

OPCODE (133, "reverb/sinks", reverb_sinks)
    ARG (A_REG + T_REVERB, "self")
    ARG (A_NAT, "w")
    ARG (A_NAT, "loffs")
    ARG (A_DOUBLE, "lg")
    ARG (A_DOUBLE, "lp")
    ARG (A_NAT, "roffs")
    ARG (A_DOUBLE, "rg")
    ARG (A_DOUBLE, "rp")
END

OPCODE (134, "reverb/diffuse", reverb_diffuse)
    ARG (A_REG + T_REVERB, "self")
    ARG (A_NAT, "w")
    ARG (A_NAT, "nstages")
    ARG (A_NAT, "l1")
    ARG (A_DOUBLE, "g1")
    ARG (A_NAT, "l2")
    ARG (A_DOUBLE, "g2")
    ARG (A_NAT, "l3")
    ARG (A_DOUBLE, "g3")
    ARG (A_NAT, "l4")
    ARG (A_DOUBLE, "g4")
END

OPCODE (135, "reverb/tcfilter", reverb_tcfilter)
    ARG (A_REG + T_REVERB, "self")
    ARG (A_DOUBLE, "beta")
END

// }}}

// vim:fdm=marker:
