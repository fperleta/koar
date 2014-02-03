
/*
 * koar/patchvm.c
 * copyright (c) 2014 Frano Perleta
 */

#include "patchvm.h"

// types {{{

struct patchvm_s {
    patch_t patch;

    size_t nregs;
    reg_t* regs;
    size_t now;

    int fail;
    int leave;
};

// }}}

// patchvm_create {{{

patchvm_t
patchvm_create (size_t nworkers, size_t nregs)
{
    patchvm_t vm = xmalloc (sizeof (struct patchvm_s));

    vm->patch = patch_create (nworkers);

    vm->nregs = nregs;
    vm->regs = xmalloc (sizeof (reg_t) * nregs);
    vm->now = 0;

    size_t i;
    for (i = 0; i < nregs; i++)
        vm->regs[i] = (reg_t) { .tag = T_BLANK, .uintptr = 0 };

    vm->fail = vm->leave = 0;

    return vm;
}

// }}}

// patchvm_destroy {{{

void
patchvm_destroy (patchvm_t vm)
{
    patch_destroy (vm->patch);
    free (vm);
}

// }}}

// accessors {{{

patch_t
patchvm_patch (patchvm_t vm)
{ return vm->patch; }

int
patchvm_failed (patchvm_t vm)
{ return vm->fail; }

int
patchvm_leaving (patchvm_t vm)
{ return vm->leave; }

// }}}

// opcodes {{{

#define MAX_ARGTAGS 8

typedef struct {
    unsigned opc;
    const char* name;
    argtag_t tags[MAX_ARGTAGS];
} opcode_t;

static const opcode_t opcodes[] = {
#define OPCODE(opc, name, code) \
    { opc, name, {
#define ARG(tag) \
    tag,
#define END \
    A_STOP } },
#include "patchvm-opcodes.h"
#undef OPCODE
#undef ARG
#undef END
};
#define NUM_OPCODES (sizeof (opcodes) / sizeof (opcode_t))

// }}}

// decoding {{{

typedef struct decoder_s* decoder_t;
struct decoder_s {
    const uint8_t* p;
    const uint8_t* end;
    int fail;
};

// naturals {{{

// 7-bit right-aligned chunks in big endian order.
// bit 7 is set on all nonfinal bytes.
static unsigned
decode_nat (decoder_t dec)
{
    unsigned x = 0;
    uint8_t b;

    if (dec->fail)
        goto fail;

    do {
        if (dec->p >= dec->end)
            goto fail;
        b = *(dec->p++);
        x <<= 1;
        x |= b & 0x7F;
    } while (b & 0x80);

fail:
    dec->fail = 1;
    return 0;
}

// }}}

// integers {{{

// nonnegative ints (0, 1, 2, ...)  <->  even nats (0, 2, 4, ...)
// negative ints (-1, -2, -3, ...)  <->  odd nats (1, 3, 5, ...)
static int
decode_int (decoder_t dec)
{
    unsigned x = decode_nat (dec);
    return (x&1)? -(int) ((x+1) >> 1) : (int) (x >> 1);
}

// }}}

// doubles {{{

// IEEE 754 binary64 in big endian order.
static double
decode_double (decoder_t dec)
{
    union {
        uint64_t u;
        double d;
    } x;
    size_t i;

    if (dec->fail)
        goto fail;

    if (dec->p + 8 > dec->end)
        goto fail;

    x.u = 0;
    for (i = 0; i < 8; i++)
        x.u |= ((uint64_t) dec->p[i]) << (i << 3);

    return x.d;

fail:
    dec->fail = 1;
    return 0;
}

// }}}

// utf8 strings {{{

static uint8_t*
decode_utf8 (decoder_t dec)
{
    uint8_t* buf = NULL;

    if (dec->p >= dec->end)
        goto fail;

    size_t len = decode_nat (dec);

    if (dec->p + len > dec->end)
        goto fail;

    buf = xmalloc (len + 1);

    size_t i;
    for (i = 0; i < len; i++)
        buf[i] = dec->p[i];
    buf[len] = 0;

    return buf;

fail:
    if (buf)
        free (buf);
    return NULL;
}

// }}}

// instructions {{{

static instr_t
decode_instr (decoder_t dec)
{
    instr_t instr = NULL;

    if (dec->p >= dec->end)
        goto fail;

    unsigned opc = decode_nat (dec);

    if (opc >= NUM_OPCODES)
        goto fail;

    arg_t* next_arg (size_t k, const argtag_t* tags)
    {
        argtag_t tag = *tags;

        if (dec->fail)
            goto fail;

        switch (tag)
        {
            case A_STOP: // {{{
                {
                    size_t nargs = k;
                    instr = xmalloc (__builtin_offsetof (struct instr_s, args)
                                     + nargs * sizeof (arg_t));
                    instr->opc = opc;
                    instr->nargs = nargs;
                    return instr->args;
                } // }}}

            case A_NAT: // {{{
                {
                    unsigned nat = decode_nat (dec);
                    arg_t* args = next_arg (k + 1, tags + 1);
                    if (dec->fail)
                        goto fail;
                    args[k].tag = tag;
                    args[k].nat = nat;
                    return args;
                } // }}}

            case A_INT: // {{{
                {
                    int int_ = decode_int (dec);
                    arg_t* args = next_arg (k + 1, tags + 1);
                    if (dec->fail)
                        goto fail;
                    args[k].tag = tag;
                    args[k].int_ = int_;
                    return args;
                } // }}}

            case A_DOUBLE:
                {
                    int dbl = decode_double (dec);
                    arg_t* args = next_arg (k + 1, tags + 1);
                    if (dec->fail)
                        goto fail;
                    args[k].tag = tag;
                    args[k].dbl = dbl;
                    return args;
                }

            case A_UTF8: // {{{
                {
                    uint8_t* utf8 = decode_utf8 (dec);
                    arg_t* args = next_arg (k + 1, tags + 1);
                    if (dec->fail)
                    {
                        if (utf8)
                            free (utf8);
                        goto fail;
                    }
                    args[k].tag = tag;
                    args[k].utf8 = utf8;
                    return args;
                } // }}}

            default: // {{{
                {
                    unsigned reg = decode_nat (dec);
                    arg_t* args = next_arg (k + 1, tags + 1);
                    if (dec->fail)
                        goto fail;
                    args[k].tag = tag;
                    args[k].reg = reg;
                    return args;
                } // }}}
        }

fail:
        dec->fail = 1;
        return NULL;
    }

    next_arg (0, opcodes[opc].tags);
    if (dec->fail)
        goto fail;
    return instr;

fail:
    dec->fail = 1;
    return NULL;
}

// }}}

// }}}

// checking {{{

typedef struct checker_s* checker_t;
struct checker_s {
    int fail;
};

static void
check_arg (patchvm_t vm, checker_t chk, arg_t arg)
{
    if (chk->fail)
        goto fail;

    if (arg.tag >= A_ANYREG)
    {
        if (arg.reg >= vm->nregs)
            goto fail;
        if ((arg.tag > A_ANYREG) && (vm->regs[arg.reg].tag != arg.tag - A_REG))
            goto fail;
    }

    return;

fail:
    chk->fail = 1;
}

static void
check_instr (patchvm_t vm, checker_t chk, instr_t instr)
{
    if (chk->fail)
        goto fail;

    if (instr->opc >= NUM_OPCODES)
        goto fail;

    size_t i;
    for (i = 0; i < instr->nargs; i++)
        check_arg (vm, chk, instr->args[i]);

    return;

fail:
    chk->fail = 1;
}

// }}}

// freeing instructions {{{

static void
free_instr (instr_t instr)
{
    if (!instr)
        return;

    size_t i;
    for (i = 0; i < instr->nargs; i++)
        if (instr->args[i].tag == A_UTF8)
            free (instr->args[i].utf8);

    free (instr);
}

// }}}

// execution {{{

// dispatch table {{{

#define OPCODE(opc, name, code) \
    extern void PATCHVM_ ## code (patchvm_t, instr_t);
#define ARG(tag)
#define END
#include "patchvm-opcodes.h"
#undef OPCODE
#undef ARG
#undef END

static patchvm_opcode_t dispatch[] = {
#define OPCODE(opc, name, code) \
    PATCHVM_ ## code,
#define ARG(tag)
#define END
#include "patchvm-opcodes.h"
#undef OPCODE
#undef ARG
#undef END
};

// }}}

// primitives {{{

void
patchvm_blank (patchvm_t vm, unsigned reg)
{
    vm->regs[reg] = (reg_t) { .tag = T_BLANK, .uintptr = 0 };
}

reg_t
patchvm_get (patchvm_t vm, unsigned reg)
{
    return vm->regs[reg];
}

void
patchvm_set (patchvm_t vm, unsigned reg, reg_t val)
{
    patchvm_blank (vm, reg);
    vm->regs[reg] = val;
}

// }}}

// builtins {{{

void
PATCHVM_nop (patchvm_t vm UNUSED, instr_t instr UNUSED)
{
}

void
PATCHVM_leave (patchvm_t vm, instr_t instr UNUSED)
{
    vm->leave = 1;
}

void
PATCHVM_resize (patchvm_t vm, instr_t instr)
{
    size_t new_nregs = instr->args[0].nat;
    size_t old_nregs = vm->nregs;
    size_t i;

    if (new_nregs < old_nregs)
        for (i = new_nregs; i < old_nregs; i++)
            patchvm_blank (vm, i);

    vm->nregs = new_nregs;
    vm->regs = xrealloc (vm->regs, sizeof (reg_t) * vm->nregs);

    if (new_nregs > old_nregs)
        for (i = old_nregs; i < new_nregs; i++)
            vm->regs[i] = (reg_t) { .tag = T_BLANK, .uintptr = 0 };
}

void
PATCHVM_blank (patchvm_t vm, instr_t instr)
{
    patchvm_blank (vm, instr->args[0].reg);
}

void
PATCHVM_move (patchvm_t vm, instr_t instr)
{
    size_t src = instr->args[0].reg;
    size_t dst = instr->args[1].reg;

    patchvm_blank (vm, dst);
    vm->regs[dst] = vm->regs[src];
    patchvm_blank (vm, src);
}

void
PATCHVM_dup (patchvm_t vm, instr_t instr)
{
    size_t src = instr->args[0].reg;
    size_t dst = instr->args[1].reg;

    patchvm_blank (vm, dst);
    vm->regs[dst] = vm->regs[src];
}

void
PATCHVM_advance (patchvm_t vm, instr_t instr)
{
    size_t dt = instr->args[0].nat;

    vm->now += dt;
}

// }}}

void
patchvm_exec (patchvm_t vm, const uint8_t* buf, size_t len)
{
    if (vm->fail)
        goto fail;

    struct decoder_s dec = {
        .p = buf, .end = buf + len,
        .fail = 0
    };

    while (dec.p < dec.end)
    {
        instr_t instr = decode_instr (&dec);
        if (dec.fail)
            goto fail_instr;

        struct checker_s chk = { .fail = 0 };
        check_instr (vm, &chk, instr);
        if (chk.fail)
            goto fail_instr;

        patchvm_opcode_t run = dispatch[instr->opc];
        run (vm, instr);
        if (vm->fail)
            goto fail_instr;

        continue;

fail_instr:
        if (instr)
            free_instr (instr);
        goto fail;
    }

    return;

fail:
    vm->fail = 1;
}

// }}}

// vim:fdm=marker:
