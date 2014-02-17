
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

// {{{
RESERVED (21)
RESERVED (22)
RESERVED (23)
RESERVED (24)
RESERVED (25)
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
    ARG (A_REG + T_PNODE, "src")
END

OPCODE (77, "fwriter1/close", fwriter_close)
    ARG (A_REG + T_FWRITER1, "self")
END

OPCODE (78, "fwriter2/make", fwriter2_make)
    ARG (A_ANYREG, "self")
    ARG (A_UTF8, "filepath")
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

// {{{
RESERVED (84)
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

// vim:fdm=marker:
