
/*
 * koar/nodes/phasor.c
 * copyright (c) 2014 Frano Perleta
 */

#include "buf.h"
#include "patchvm.h"
#include "nodes/phasor.h"

// state {{{

typedef struct phasor_s* phasor_t;

struct phasor_s {
    samp_t phase;
};

// }}}

// callbacks {{{

static void
phasor_tick (patch_t p, anode_t an, patch_stamp_t now, size_t delta)
{
    phasor_t ph = anode_state (an);
    pnode_t src = anode_get_source (an, 0);
    pnode_t snk = anode_get_sink (an, 0);

    buf_t in = { .p = pnode_read (src, now).p };
    buf_t b = buf_alloc (p->bufpool);

    samp_t phase = ph->phase;
    size_t i;
    for (i = 0; i < delta; i++)
    {
        b.xs[i] = phase;
        phase += in.xs[i];
        phase = phase - floor (phase);
    }
    ph->phase = phase;
    buf_release (in);

    patch_datum_t x = { .b = b };
    pnode_write (p, snk, x, now);
}

static struct ainfo_s
phasor_ainfo = {
    .init = NULL,
    .exit = NULL,
    .tick = phasor_tick,
    .ins = 1,
    .outs = 1,
    .size = sizeof (struct phasor_s)
};

// }}}

// make {{{

anode_t
N_phasor_make (patch_t p, pnode_t src, pnode_t snk)
{
    anode_t an = anode_create (p, &phasor_ainfo);
    anode_source (an, 0, src);
    anode_sink (an, 0, snk);

    phasor_t ph = anode_state (an);
    ph->phase = 0;

    return an;
}

void
PATCHVM_phasor_make (patchvm_t vm, instr_t instr)
{
    patch_t p = patchvm_patch (vm);
    pnode_t src = patchvm_get (vm, instr->args[1].reg).pn;
    pnode_t snk = patchvm_get (vm, instr->args[2].reg).pn;
    reg_t val = { .tag = T_PHASOR, .an = N_phasor_make (p, src, snk) };
    patchvm_set (vm, instr->args[0].reg, val);
}

// }}}

// jump {{{

void
N_phasor_jump (anode_t an, samp_t phase)
{
    anode_lock (an);

    phasor_t ph = anode_state (an);
    ph->phase = phase - floor (phase);

    anode_unlock (an);
}

void
PATCHVM_phasor_jump (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    double phase = instr->args[0].dbl;
    N_phasor_jump (an, phase);
}

// }}}

// vim:fdm=marker:
