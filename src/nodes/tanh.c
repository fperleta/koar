
/*
 * koar/nodes/tanh.c
 * copyright (c) 2014 Frano Perleta
 */

#include "buf.h"
#include "patchvm.h"
#include "nodes/active.h"

// state {{{

typedef struct tanh_s* tanh_t;

struct tanh_s {
    samp_t slope;
    samp_t gain;
};

// }}}

// processing loop {{{

static void __attribute__ ((hot))
tanh_loop (tanh_t th, const samp_t* xs, samp_t* ys, size_t len)
{
    size_t i;
    samp_t a = th->slope;
    samp_t g = th->gain;

    for (i = 0; i < len; i++)
        ys[i] = g * tanh (a * xs[i]);
}

// }}}

// callbacks {{{

static void
tanh_tick (patch_t p, anode_t an, patch_stamp_t now, size_t delta)
{
    tanh_t th = anode_state (an);

    pnode_t src = anode_get_source (an, 0);
    buf_t in = { .p = pnode_read (src, now).p };

    buf_t b = buf_alloc (p->bufpool);

    tanh_loop (th, in.xs, b.xs, delta);

    buf_release (in);

    pnode_t snk = anode_get_sink (an, 0);
    patch_datum_t out = { .b = b };
    pnode_write (p, snk, out, now);
}

static struct ainfo_s
tanh_ainfo = {
    .name = "tanh",
    .init = NULL,
    .exit = NULL,
    .tick = tanh_tick,
    .ins = 1,
    .outs = 1,
    .size = sizeof (struct tanh_s)
};

// }}}

// make {{{

anode_t
N_tanh_make (patch_t p, pnode_t src, pnode_t snk)
{
    anode_t an = anode_create (p, &tanh_ainfo);
    anode_source (an, 0, src);
    anode_sink (an, 0, snk);

    tanh_t th = anode_state (an);
    th->slope = 1;
    th->gain = 1;

    return an;
}

void
PATCHVM_tanh_make (patchvm_t vm, instr_t instr)
{
    patch_t p = patchvm_patch (vm);
    pnode_t src = patchvm_get (vm, instr->args[1].reg).pn;
    pnode_t snk = patchvm_get (vm, instr->args[2].reg).pn;
    reg_t val = { .tag = T_TANH, .an = N_tanh_make (p, src, snk) };
    patchvm_set (vm, instr->args[0].reg, val);
}

// }}}

// gain {{{

void
N_tanh_gain (anode_t an, samp_t gain)
{
    tanh_t th = anode_state (an);
    th->gain = gain;
}

void
PATCHVM_tanh_gain (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    samp_t gain = instr->args[1].dbl;
    N_tanh_gain (an, gain);
}

// }}}

// slope {{{

void
N_tanh_slope (anode_t an, samp_t slope)
{
    tanh_t th = anode_state (an);
    th->slope = slope;
}

void
PATCHVM_tanh_slope (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    samp_t slope = instr->args[1].dbl;
    N_tanh_slope (an, slope);
}

// }}}

// vim:fdm=marker:
