
/*
 * koar/patchvm.h
 * copyright (c) 2014 Frano Perleta
 */

#ifndef KOAR_PATCHVM_H
#define KOAR_PATCHVM_H

#include "patch.h"
#include "defs.h"

// types {{{

typedef enum {
    T_BLANK = 0,
    T_PNODE,
    T_ENV,

    NUM_REGTAGS
} regtag_t;

typedef enum {
    A_STOP = 0,
    A_NAT,
    A_INT,
    A_DOUBLE,
    // keep these two at the end.
    A_ANYREG,
    A_REG
} argtag_t;

typedef struct {
    regtag_t tag;
    union {
        uintptr_t uintptr;
        void* ptr;
        pnode_t pn;
        anode_t an;
    };
} reg_t;

typedef struct {
    argtag_t tag;
    union {
        unsigned nat;
        int int_;
        unsigned reg;
        double dbl;
    };
} arg_t;

typedef struct instr_s* instr_t;
struct instr_s {
    unsigned opc;
    size_t nargs;
    arg_t args[];
};

typedef struct patchvm_s* patchvm_t;

typedef void (*patchvm_opcode_t) (patchvm_t, instr_t);

// }}}

extern patchvm_t patchvm_create (size_t, size_t);
extern void patchvm_destroy (patchvm_t);

extern patch_t patchvm_patch (patchvm_t);
extern int patchvm_failed (patchvm_t);
extern int patchvm_leaving (patchvm_t);

extern void patchvm_blank (patchvm_t vm, unsigned);
extern reg_t patchvm_get (patchvm_t vm, unsigned);
extern void patchvm_set (patchvm_t vm, unsigned, reg_t);

extern void patchvm_exec (patchvm_t vm, const uint8_t*, size_t);

#endif /* koar/patchvm.h */

// vim:fdm=marker:
