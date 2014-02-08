
/*
 * koar/nodes/passive.c
 * copright (c) 2014 Frano Perleta
 */

#include "buf.h"
#include "patchvm.h"
#include "nodes/passive.h"

// utilities {{{

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

static patch_datum_t
sum_pass (patch_t p, patch_datum_t a)
{
    if (a.p)
    {
        buf_acquire (a.b);
        return a;
    }
    else
    {
        buf_t b = buf_alloc (p->bufpool);
        buf_const (b, 0, BUF_SAMPLES);
        patch_datum_t val = { .b = b };
        return val;
    }
}

static struct pinfo_s
sum_pinfo = {
    .neutral = { .p = NULL },
    .combine = sum_combine,
    .pass = sum_pass,
    .dispose = dispose
};

pnode_t
N_sum (patch_t p)
{
    return pnode_create (p, &sum_pinfo);
}

void
PATCHVM_sum (patchvm_t vm, instr_t instr)
{
    patch_t p = patchvm_patch (vm);
    reg_t val = { .tag = T_PNODE, .pn = N_sum (p) };
    patchvm_set (vm, instr->args[0].reg, val);
}

// }}}

// N_prod {{{

static patch_datum_t
prod_combine (patch_t p, patch_datum_t a, patch_datum_t b)
{
    buf_t x = (buf_t) a.p, y = (buf_t) b.p;

    if (!x.p)
        return b;
    if (!y.p)
        return a;

    buf_mul (x, y, p->delta);
    buf_release (y);

    return a;
}

static patch_datum_t
prod_pass (patch_t p, patch_datum_t a)
{
    if (a.p)
    {
        buf_acquire (a.b);
        return a;
    }
    else
    {
        buf_t b = buf_alloc (p->bufpool);
        buf_const (b, 1, p->delta);
        patch_datum_t val = { .b = b };
        return val;
    }
}

static struct pinfo_s
prod_pinfo = {
    .neutral = { .p = NULL },
    .combine = prod_combine,
    .pass = prod_pass,
    .dispose = dispose
};

pnode_t
N_prod (patch_t p)
{
    return pnode_create (p, &prod_pinfo);
}

void
PATCHVM_prod (patchvm_t vm, instr_t instr)
{
    patch_t p = patchvm_patch (vm);
    reg_t val = { .tag = T_PNODE, .pn = N_prod (p) };
    patchvm_set (vm, instr->args[0].reg, val);
}


// }}}

// vim:fdm=marker:
