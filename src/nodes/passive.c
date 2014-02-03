
/*
 * koar/nodes/passive.c
 * copright (c) 2014 Frano Perleta
 */

#include "buf.h"
#include "patchvm.h"
#include "nodes/passive.h"

// utilities {{{

static patch_datum_t
pass (patch_datum_t a)
{
    buf_acquire (a.b);
    return a;
}

static void
dispose (patch_datum_t a)
{
    buf_t x = (buf_t) a.p;
    if (x.p)
        buf_release (x);
}

// }}}

// N_sum {{{

static patch_datum_t
sum_combine (patch_t p, patch_datum_t a, patch_datum_t b)
{
    buf_t x = (buf_t) a.p, y = (buf_t) b.p;

    if (!x.p)
        return b;
    if (!y.p)
        return a;

    buf_add (x, y, p->delta);
    buf_release (y);

    return a;
}

static struct pinfo_s
sum_pinfo = {
    .neutral = { .p = NULL },
    .combine = sum_combine,
    .pass = pass,
    .dispose = dispose
};

pnode_t
N_sum (void)
{
    return pnode_create (&sum_pinfo);
}

void
PATCHVM_sum (patchvm_t vm, instr_t instr)
{
    reg_t val = { .tag = T_PNODE, .pn = N_sum () };
    patchvm_set (vm, instr->args[0].reg, val);
}

// }}}

// N_prod {{{

static patch_datum_t
prod_combine (patch_t p, patch_datum_t a, patch_datum_t b)
{
    buf_t x = (buf_t) a.p, y = (buf_t) b.p;

    if (!x.p || !y.p)
        return (patch_datum_t) { .p = NULL };

    buf_mul (x, y, p->delta);
    buf_release (y);
    return a;
}

static struct pinfo_s
prod_pinfo = {
    .neutral = { .p = NULL },
    .combine = prod_combine,
    .pass = pass,
    .dispose = dispose
};

pnode_t
N_prod (void)
{
    return pnode_create (&prod_pinfo);
}

void
PATCHVM_prod (patchvm_t vm, instr_t instr)
{
    reg_t val = { .tag = T_PNODE, .pn = N_prod () };
    patchvm_set (vm, instr->args[0].reg, val);
}


// }}}

// vim:fdm=marker:
