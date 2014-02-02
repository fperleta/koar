
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

// vim:fdm=marker:
