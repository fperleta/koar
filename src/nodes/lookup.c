
/*
 * koar/nodes/lookup.c
 * copyright (c) 2014 Frano Perleta
 */

#include "array.h"
#include "patchvm.h"
#include "defs.h"

// state {{{

typedef struct lookup_s* lookup_t;

struct lookup_s {
    array_t arr;
};

// }}}

// callbacks {{{

static void
lookup_tick (patch_t p, anode_t an, patch_stamp_t now, size_t delta)
{
    lookup_t lu = anode_state (an);
    pnode_t src = anode_get_source (an, 0);
    pnode_t snk = anode_get_sink (an, 0);

    buf_t in = { .p = pnode_read (src, now).p };
    buf_t b = buf_alloc (p->bufpool);
    array_lookup (lu->arr, in.xs, b.xs, delta);
    buf_release (in);

    patch_datum_t x = { .b = b };
    pnode_write (p, snk, x, now);
}

static void
lookup_exit (anode_t an)
{
    lookup_t lu = anode_state (an);
    if (lu->arr)
        array_release (lu->arr);
}

static struct ainfo_s
lookup_ainfo = {
    .name = "lookup",
    .init = NULL,
    .exit = lookup_exit,
    .tick = lookup_tick,
    .ins = 1,
    .outs = 1,
    .size = sizeof (struct lookup_s)
};

// }}}

// make {{{

anode_t
N_lookup_make (patch_t p, array_t arr, pnode_t src, pnode_t snk)
{
    anode_t an = anode_create (p, &lookup_ainfo);
    anode_source (an, 0, src);
    anode_sink (an, 0, snk);

    lookup_t lu = anode_state (an);
    lu->arr = array_acquire (arr);

    return an;
}

void
PATCHVM_lookup_make (patchvm_t vm, instr_t instr)
{
    patch_t p = patchvm_patch (vm);
    array_t arr = patchvm_get (vm, instr->args[1].reg).arr;
    pnode_t src = patchvm_get (vm, instr->args[2].reg).pn;
    pnode_t snk = patchvm_get (vm, instr->args[3].reg).pn;
    reg_t val = { .tag = T_LOOKUP, .an = N_lookup_make (p, arr, src, snk) };
    patchvm_set (vm, instr->args[0].reg, val);
}

// }}}

// table {{{

void
N_lookup_table (anode_t an, array_t arr)
{
    anode_lock (an);

    lookup_t lu = anode_state (an);
    if (lu->arr)
        array_release (lu->arr);

    lu->arr = array_acquire (arr);

    anode_unlock (an);
}

void
PATCHVM_lookup_table (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    array_t arr = patchvm_get (vm, instr->args[1].reg).arr;
    N_lookup_table (an, arr);
}

// }}}

// vim:fdm=marker:
