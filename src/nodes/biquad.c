
/*
 * koar/nodes/biquad.c
 * copyright (c) 2014 Frano Perleta
 */

/*
 * A `biquad` node implements a cascade of biquad filters with fixed coefficients.
 * The transfer function is
 *
 *      H(z) = g H_1(z) ... H_n(z)
 *
 * where `g` is the overall gain, and each `H_i` is of the form
 *
 *             1 + b₁/z + b₂/z²
 *      H(z) = ---------------- .
 *             1 + a₁/z + a₂/z²
 *
 * Each biquad stage is defined by a difference equation:
 *
 *      y[n] = x[n] + b₁ x[n-1] + b₁ x[n-2]
 *                  - a₁ y[n-1] - a₂ y[n-2]
 *
 * Transposed Direct Form II is used for numerical robustness:
 *
 *      y[n]   =  x[n] + h₁[n-1]
 *      h₁[n]  =  b₁ x[n] - a₁ y[n] + h₂[n-1]
 *      h₂[n]  =  b₂ x[n] - a₂ y[n]
 *
 * This means that only two state variables (h₁ and h₂) must be maintained.
 */

#include "buf.h"
#include "patchvm.h"
#include "nodes/active.h"

// state {{{

typedef struct biquad_s* biquad_t;

struct stage_s {
    samp_t b1, b2;
    samp_t a1, a2;
    samp_t h1, h2;
};

struct biquad_s {
    size_t nstages;
    struct stage_s* stages;
    samp_t gain;
};

// }}}

// processing loop {{{

static void __attribute__ ((hot))
bq_loop (biquad_t bq, const samp_t* xs, samp_t* ys, size_t len)
{
    size_t i, j;

    for (i = 0; i < len; i++)
        ys[i] = bq->gain * xs[i];

    for (j = 0; j < bq->nstages; j++)
    {
        struct stage_s* s = bq->stages + j;

        samp_t h1 = s->h1, h2 = s->h2;
        for (i = 0; i < len; i++)
        {
            samp_t x = ys[i];
            samp_t y = x + h1;
            h1 = s->b1 * x - s->a1 * y + h2;
            h2 = s->b2 * x - s->a2 * y;
            ys[i] = y;
        }
        s->h1 = h1;
        s->h2 = h2;
    }
}

// }}}

// callbacks {{{

static void
biquad_exit (anode_t an)
{
    biquad_t bq = anode_state (an);
    if (bq->stages)
        free (bq->stages);
    bq->stages = NULL;
}

static void
biquad_tick (patch_t p, anode_t an, patch_stamp_t now, size_t delta)
{
    biquad_t bq = anode_state (an);

    pnode_t src = anode_get_source (an, 0);
    buf_t in = { .p = pnode_read (src, now).p };

    buf_t b = buf_alloc (p->bufpool);

    bq_loop (bq, in.xs, b.xs, delta);

    buf_release (in);

    pnode_t snk = anode_get_sink (an, 0);
    patch_datum_t out = { .b = b };
    pnode_write (p, snk, out, now);
}

static struct ainfo_s
biquad_ainfo = {
    .name = "biquad",
    .init = NULL,
    .exit = biquad_exit,
    .tick = biquad_tick,
    .ins = 1,
    .outs = 1,
    .size = sizeof (struct biquad_s)
};

// }}}

// make {{{

anode_t
N_biquad_make (patch_t p, pnode_t src, pnode_t snk, size_t nstages)
{
    anode_t an = anode_create (p, &biquad_ainfo);
    anode_source (an, 0, src);
    anode_sink (an, 0, snk);

    biquad_t bq = anode_state (an);
    bq->nstages = nstages;
    bq->stages = xmalloc (sizeof (struct stage_s) * nstages);
    bq->gain = 1;

    size_t i;
    for (i = 0; i < nstages; i++)
    {
        struct stage_s* s = bq->stages + i;
        s->b1 = s->b2 = s->a1 = s->a2 = 0;
        s->h1 = s->h2 = 0;
    }

    return an;
}

void
PATCHVM_biquad_make (patchvm_t vm, instr_t instr)
{
    patch_t p = patchvm_patch (vm);
    pnode_t src = patchvm_get (vm, instr->args[1].reg).pn;
    pnode_t snk = patchvm_get (vm, instr->args[2].reg).pn;
    size_t nstages = instr->args[3].nat;
    reg_t val = { .tag = T_BIQUAD, .an = N_biquad_make (p, src, snk, nstages) };
    patchvm_set (vm, instr->args[0].reg, val);
}

// }}}

// gain {{{

void
N_biquad_gain (anode_t an, samp_t gain)
{
    biquad_t bq = anode_state (an);
    bq->gain = gain;
}

void
PATCHVM_biquad_gain (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    samp_t gain = instr->args[0].dbl;
    N_biquad_gain (an, gain);
}

// }}}

// coeffs {{{

void
N_biquad_coeffs (anode_t an, size_t i, samp_t b1, samp_t b2, samp_t a1, samp_t a2)
{
    biquad_t bq = anode_state (an);
    struct stage_s* s = bq->stages + (i % bq->nstages);
    s->b1 = b1;
    s->b2 = b2;
    s->a1 = a1;
    s->a2 = a2;
}

void
PATCHVM_biquad_coeffs (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    size_t i = instr->args[1].nat;
    samp_t b1 = instr->args[2].dbl;
    samp_t b2 = instr->args[3].dbl;
    samp_t a1 = instr->args[4].dbl;
    samp_t a2 = instr->args[5].dbl;
    N_biquad_coeffs (an, i, b1, b2, a1, a2);
}

// }}}

// vim:fdm=marker:
