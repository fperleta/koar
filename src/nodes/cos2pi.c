
/*
 * koar/nodes/cos2pi.c
 * copyright (c) 2014 Frano Perleta
 */

#include "buf.h"
#include "patchvm.h"
#include "nodes/active.h"

// callbacks {{{

static void
cos2pi_tick (patch_t p, anode_t an, patch_stamp_t now, size_t delta)
{
    pnode_t src = anode_get_source (an, 0);
    pnode_t snk = anode_get_sink (an, 0);

    buf_t in = { .p = pnode_read (src, now).p };
    buf_t b = buf_alloc (p->bufpool);
    buf_cos2pi (b, in, delta);
    buf_release (in);

    patch_datum_t x = { .b = b };
    pnode_write (p, snk, x, now);
}

static struct ainfo_s
cos2pi_ainfo = {
    .name = "cos2pi",
    .init = NULL,
    .exit = NULL,
    .tick = cos2pi_tick,
    .ins = 1,
    .outs = 1,
    .size = 0
};

// }}}

// make {{{

anode_t
N_cos2pi_make (patch_t p, pnode_t src, pnode_t snk)
{
    anode_t an = anode_create (p, &cos2pi_ainfo);
    anode_source (an, 0, src);
    anode_sink (an, 0, snk);
    return an;
}

void
PATCHVM_cos2pi_make (patchvm_t vm, instr_t instr)
{
    patch_t p = patchvm_patch (vm);
    pnode_t src = patchvm_get (vm, instr->args[1].reg).pn;
    pnode_t snk = patchvm_get (vm, instr->args[2].reg).pn;
    reg_t val = { .tag = T_COS2PI, .an = N_cos2pi_make (p, src, snk) };
    patchvm_set (vm, instr->args[0].reg, val);
}

// }}}

// vim:fdm=marker:
