
/*
 * koar/nodes/wire.c
 * copyright (c) 2014 Frano Perleta
 */

#include "buf.h"
#include "patchvm.h"
#include "nodes/active.h"

// state {{{

typedef struct wire_s* wire_t;

struct wire_s {
    double scale;
};

// }}}

// callbacks {{{

static void
wire_tick (patch_t p, anode_t an, patch_stamp_t now, size_t delta)
{
    wire_t w = anode_state (an);
    pnode_t src = anode_get_source (an, 0);
    pnode_t snk = anode_get_sink (an, 0);

    buf_t in = { .p = pnode_read (src, now).p };
    buf_t b = buf_alloc (p->bufpool);
    buf_scale (b, in, w->scale, delta);
    buf_release (in);

    patch_datum_t x = { .b = b };
    pnode_write (p, snk, x, now);
}

static struct ainfo_s
wire_ainfo = {
    .name = "wire",
    .init = NULL,
    .exit = NULL,
    .tick = wire_tick,
    .ins = 1,
    .outs = 1,
    .size = sizeof (struct wire_s)
};

// }}}

// make {{{

anode_t
N_wire_make (patch_t p, pnode_t in, pnode_t out, double scale)
{
    anode_t an = anode_create (p, &wire_ainfo);
    anode_source (an, 0, in);
    anode_sink (an, 0, out);

    wire_t w = anode_state (an);
    w->scale = scale;

    return an;
}

void
PATCHVM_wire_make (patchvm_t vm, instr_t instr)
{
    patch_t p = patchvm_patch (vm);
    pnode_t in = patchvm_get (vm, instr->args[1].reg).pn;
    pnode_t out = patchvm_get (vm, instr->args[2].reg).pn;
    double scale = instr->args[3].dbl;
    reg_t val = { .tag = T_WIRE, .an = N_wire_make (p, in, out, scale) };
    patchvm_set (vm, instr->args[0].reg, val);
}

// }}}

// scale {{{

void
N_wire_scale (anode_t an, double scale)
{
    anode_lock (an);

    wire_t w = anode_state (an);
    w->scale = scale;

    anode_unlock (an);
}

void
PATCHVM_wire_scale (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    double scale = instr->args[1].dbl;
    N_wire_scale (an, scale);
}

// }}}

// vim:fdm=marker:
