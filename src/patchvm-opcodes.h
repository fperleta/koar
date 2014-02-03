
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

OPCODE (18, "fwriter1/make", fwriter1_make)
    ARG (A_ANYREG)
    ARG (A_UTF8)
    ARG (A_REG + T_PNODE)
END

OPCODE (19, "fwriter1/close", fwriter_close)
    ARG (A_REG + T_FWRITER1)
END

OPCODE (20, "fwriter2/make", fwriter2_make)
    ARG (A_ANYREG)
    ARG (A_UTF8)
    ARG (A_REG + T_PNODE)
    ARG (A_REG + T_PNODE)
END

OPCODE (21, "fwriter2/close", fwriter_close)
    ARG (A_REG + T_FWRITER2)
END

OPCODE (22, "fwriter2/make", fwriter2_make)
    ARG (A_ANYREG)
    ARG (A_UTF8)
    ARG (A_REG + T_PNODE)
    ARG (A_REG + T_PNODE)
END

OPCODE (23, "env/make", env_make)
    ARG (A_ANYREG)
    ARG (A_REG + T_PNODE)
    ARG (A_DOUBLE)
END

OPCODE (24, "env/const", env_const)
    ARG (A_REG + T_ENV)
    ARG (A_DOUBLE)
END

OPCODE (25, "env/lin", env_lin)
    ARG (A_REG + T_ENV)
    ARG (A_DOUBLE)
    ARG (A_DOUBLE)
END

// vim:fdm=marker:
