
/*
 * koar/patchvm-opcodes.h
 * copyright (c) 2014 Frano Perleta
 */

// (0 - 7) basic instructions {{{

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

OPCODE (7, "reserved", nop)
END

// }}}

// (8 - 15) passive nodes {{{

OPCODE (8, "sum", sum)
    ARG (A_ANYREG, "dst")
END

OPCODE (9, "prod", prod)
    ARG (A_ANYREG, "dst")
END

OPCODE (10, "reserved", nop)
END

OPCODE (11, "reserved", nop)
END

OPCODE (12, "reserved", nop)
END

OPCODE (13, "reserved", nop)
END

OPCODE (14, "reserved", nop)
END

OPCODE (15, "reserved", nop)
END

// }}}

// (16 - 17) wires {{{

OPCODE (16, "wire/make", wire_make)
    ARG (A_ANYREG, "self")
    ARG (A_REG + T_PNODE, "src")
    ARG (A_REG + T_PNODE, "snk")
    ARG (A_DOUBLE, "scale")
END

OPCODE (17, "wire/scale", wire_scale)
    ARG (A_REG + T_WIRE, "self")
    ARG (A_DOUBLE, "scale")
END

// }}}

// (18 - 21) fwriters {{{

OPCODE (18, "fwriter1/make", fwriter1_make)
    ARG (A_ANYREG, "self")
    ARG (A_UTF8, "filepath")
    ARG (A_REG + T_PNODE, "src")
END

OPCODE (19, "fwriter1/close", fwriter_close)
    ARG (A_REG + T_FWRITER1, "self")
END

OPCODE (20, "fwriter2/make", fwriter2_make)
    ARG (A_ANYREG, "self")
    ARG (A_UTF8, "filepath")
    ARG (A_REG + T_PNODE, "left")
    ARG (A_REG + T_PNODE, "right")
END

OPCODE (21, "fwriter2/close", fwriter_close)
    ARG (A_REG + T_FWRITER2, "self")
END

// }}}

// (22 - 27) envs {{{

OPCODE (22, "env/make", env_make)
    ARG (A_ANYREG, "self")
    ARG (A_REG + T_PNODE, "snk")
    ARG (A_DOUBLE, "x0")
END

OPCODE (23, "env/const", env_const)
    ARG (A_REG + T_ENV, "self")
    ARG (A_DOUBLE, "x0")
END

OPCODE (24, "env/lin", env_lin)
    ARG (A_REG + T_ENV, "self")
    ARG (A_DOUBLE, "x1")
    ARG (A_DOUBLE, "t")
END

OPCODE (25, "env/xdec", env_xdec)
    ARG (A_REG + T_ENV, "self")
    ARG (A_DOUBLE, "xinf")
    ARG (A_DOUBLE, "tau")
END

OPCODE (26, "reserved", nop)
END

OPCODE (27, "reserved", nop)
END

// }}}

// (28 - 29) phasors {{{

OPCODE (28, "phasor/make", phasor_make)
    ARG (A_ANYREG, "self")
    ARG (A_REG + T_PNODE, "src")
    ARG (A_REG + T_PNODE, "snk")
END

OPCODE (29, "phasor/jump", phasor_jump)
    ARG (A_REG + T_PHASOR, "self")
    ARG (A_DOUBLE, "phase")
END

// }}}

// vim:fdm=marker:
