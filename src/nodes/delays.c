
/*
 * koar/nodes/delays.c
 * copyright (c) 2014 Frano Perleta
 */

#include "buf.h"
#include "delay.h"
#include "patchvm.h"
#include "nodes/active.h"

// dwriters {{{

// state {{{

typedef struct dwriter_s* dwriter_t;

struct dwriter_s {
    delay_t del;
};

// }}}

// callbacks {{{

static void
dwriter_exit (anode_t an)
{
    dwriter_t dw = anode_state (an);
    if (dw->del)
        delay_release (dw->del);
    dw->del = NULL;
}

static void
dwriter_tick (patch_t p UNUSED, anode_t an, patch_stamp_t now, size_t delta)
{
    dwriter_t dw = anode_state (an);

    pnode_t src = anode_get_source (an, 0);
    buf_t b = { .p = pnode_read (src, now).p };

    delay_push (dw->del, now, delta, b.xs);

    buf_release (b);
}

static struct ainfo_s
dwriter_ainfo = {
    .name = "dwriter",
    .init = NULL,
    .exit = dwriter_exit,
    .tick = dwriter_tick,
    .ins = 1,
    .outs = 0,
    .size = sizeof (struct dwriter_s)
};

// }}}

// make {{{

anode_t
N_dwriter_make (patch_t p, pnode_t in, size_t len)
{
    anode_t an = anode_create (p, &dwriter_ainfo);
    anode_source (an, 0, in);

    dwriter_t dw = anode_state (an);

    dw->del = delay_create (len, p->now);

    return an;
}

void
PATCHVM_dwriter_make (patchvm_t vm, instr_t instr)
{
    patch_t p = patchvm_patch (vm);
    pnode_t in = patchvm_get (vm, instr->args[1].reg).pn;
    size_t len = instr->args[2].nat;
    reg_t val = { .tag = T_DWRITER, .an = N_dwriter_make (p, in, len) };
    patchvm_set (vm, instr->args[0].reg, val);
}

// }}}

// }}}

// dtaps {{{

// state {{{

typedef struct dtap_s* dtap_t;

struct dtap_s {
    delay_t del;
    size_t offs;
};

// }}}

// callbacks {{{

static void
dtap_exit (anode_t an)
{
    dtap_t tap = anode_state (an);
    if (tap->del)
        delay_release (tap->del);
    tap->del = NULL;
}

static void
dtap_tick (patch_t p, anode_t an, patch_stamp_t now, size_t delta)
{
    dtap_t tap = anode_state (an);

    buf_t b = buf_alloc (p->bufpool);

    delay_qtap (tap->del, now - tap->offs, delta, b);

    patch_datum_t out = { .b = b };
    pnode_write (p, anode_get_sink (an, 0), out, now);
}

static struct ainfo_s
dtap_ainfo = {
    .name = "dtap",
    .init = NULL,
    .exit = dtap_exit,
    .tick = dtap_tick,
    .ins = 0,
    .outs = 1,
    .size = sizeof (struct dtap_s)
};

// }}}

// make {{{

anode_t
N_dtap_make (patch_t p, pnode_t out, anode_t from, size_t offs)
{
    anode_t an = anode_create (p, &dtap_ainfo);
    anode_sink (an, 0, out);

    dtap_t tap = anode_state (an);
    dwriter_t dw = anode_state (from);
    tap->del = delay_acquire (dw->del);
    tap->offs = offs;

    return an;
}

void
PATCHVM_dtap_make (patchvm_t vm, instr_t instr)
{
    patch_t p = patchvm_patch (vm);
    pnode_t out = patchvm_get (vm, instr->args[1].reg).pn;
    anode_t from = patchvm_get (vm, instr->args[2].reg).an;
    size_t offs = instr->args[3].nat;
    reg_t val = { .tag = T_DTAP, .an = N_dtap_make (p, out, from, offs) };
    patchvm_set (vm, instr->args[0].reg, val);
}

// }}}

// adjust {{{

void
N_dtap_adjust (anode_t an, size_t offs)
{
    dtap_t tap = anode_state (an);
    tap->offs = offs;
}

void
PATCHVM_dtap_adjust (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    size_t offs = instr->args[1].nat;
    N_dtap_adjust (an, offs);
}

// }}}

// }}}

// vim:fdm=marker:
