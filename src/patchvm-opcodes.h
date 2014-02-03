
/*
 * koar/patchvm-opcodes.h
 * copyright (c) 2014 Frano Perleta
 */

OPCODE (0, "nop", nop)
END

OPCODE (1, "leave", leave)
END

OPCODE (2, "resize", resize)
    ARG (A_NAT)
END

OPCODE (3, "blank", blank)
    ARG (A_ANYREG)
END

OPCODE (4, "move", move)
    ARG (A_ANYREG)
    ARG (A_ANYREG)
END

OPCODE (5, "dup", dup)
    ARG (A_ANYREG)
    ARG (A_ANYREG)
END

OPCODE (6, "advance", advance)
    ARG (A_NAT)
END

OPCODE (7, "reserved", nop)
END

OPCODE (8, "sum", sum)
    ARG (A_ANYREG)
END

OPCODE (9, "prod", prod)
    ARG (A_ANYREG)
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

OPCODE (16, "wire/make", wire_make)
    ARG (A_ANYREG)
    ARG (A_REG + T_PNODE)
    ARG (A_REG + T_PNODE)
    ARG (A_DOUBLE)
END

OPCODE (17, "wire/scale", wire_scale)
    ARG (A_REG + T_WIRE)
    ARG (A_DOUBLE)
END

OPCODE (18, "env/make", env_make)
    ARG (A_ANYREG)
    ARG (A_REG + T_PNODE)
    ARG (A_DOUBLE)
END

OPCODE (19, "env/const", env_const)
    ARG (A_REG + T_ENV)
    ARG (A_DOUBLE)
END

OPCODE (20, "env/lin", env_lin)
    ARG (A_REG + T_ENV)
    ARG (A_DOUBLE)
    ARG (A_DOUBLE)
END

// vim:fdm=marker:
