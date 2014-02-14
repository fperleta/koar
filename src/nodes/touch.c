
/*
 * koar/nodes/touch.c
 * copyright (c) 2014 Frano Perleta
 */

#include "patchvm.h"
#include "active.h"

// callbacks {{{

static void
touch_tick (patch_t p, anode_t an, patch_stamp_t now, size_t delta UNUSED)
{
    pnode_dont_write (p, anode_get_sink (an, 0), now);
}

static struct ainfo_s
touch_ainfo = {
    .name = "touch",
    .init = NULL,
    .exit = NULL,
    .tick = touch_tick,
    .ins = 0,
    .outs = 1,
    .size = 0
};

// }}}

// touch {{{

anode_t
N_touch (patch_t p, pnode_t out)
{
    anode_t an = anode_create (p, &touch_ainfo);
    anode_sink (an, 0, out);
    return an;
}

void
PATCHVM_touch (patchvm_t vm, instr_t instr)
{
    patch_t p = patchvm_patch (vm);
    pnode_t out = patchvm_get (vm, instr->args[1].reg).pn;
    reg_t val = { .tag = T_TOUCH, .an = N_touch (p, out) };
    patchvm_set (vm, instr->args[0].reg, val);
}

// }}}

// vim:fdm=marker:
