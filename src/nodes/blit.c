
/*
 * koar/nodes/blit.c
 * copyright (c) 2014 Frano Perleta
 */

#include "buf.h"
#include "patchvm.h"
#include "nodes/active.h"

// state {{{

typedef struct blit_s* blit_t;

struct blit_s {
    samp_t phase;
};

// }}}

// processing loop {{{

static void __attribute__ ((hot))
blit_loop (blit_t blit, const samp_t* fs, samp_t* ys, size_t len)
{
    size_t i;

    samp_t ph = blit->phase;
    for (i = 0; i < len; i++)
    {
        samp_t f = fs[i];

        samp_t m = 2 * floor (0.5 / f) + 1;

        samp_t y;
        if (unlikely (ph < 0.0001))
            y = cos (m * ph * M_PI) / cos (ph * M_PI);
        else
            y = sin (m * ph * M_PI) / (m * sin (ph * M_PI));

        ys[i] = y;
        ph += f;
        ph -= floor (ph);
    }
    blit->phase = ph;
}

// }}}

// callbacks {{{

static void
blit_tick (patch_t p, anode_t an, patch_stamp_t now, size_t delta)
{
    blit_t blit = anode_state (an);

    pnode_t freq = anode_get_source (an, 0);
    buf_t f = { .p = pnode_read (freq, now).p };

    buf_t b = buf_alloc (p->bufpool);

    blit_loop (blit, f.xs, b.xs, delta);

    buf_release (f);

    pnode_t snk = anode_get_sink (an, 0);
    patch_datum_t out = { .b = b };
    pnode_write (p, snk, out, now);
}

static struct ainfo_s
blit_ainfo = {
    .name = "blit",
    .init = NULL,
    .exit = NULL,
    .tick = blit_tick,
    .ins = 1,
    .outs = 1,
    .size = sizeof (struct blit_s)
};

// }}}

// make {{{

anode_t
N_blit_make (patch_t p, pnode_t freq, pnode_t snk)
{
    anode_t an = anode_create (p, &blit_ainfo);
    anode_source (an, 0, freq);
    anode_sink (an, 0, snk);

    blit_t blit = anode_state (an);
    blit->phase = 0;

    return an;
}

void
PATCHVM_blit_make (patchvm_t vm, instr_t instr)
{
    patch_t p = patchvm_patch (vm);
    pnode_t freq = patchvm_get (vm, instr->args[1].reg).pn;
    pnode_t snk = patchvm_get (vm, instr->args[2].reg).pn;
    reg_t val = { .tag = T_BLIT, .an = N_blit_make (p, freq, snk) };
    patchvm_set (vm, instr->args[0].reg, val);
}

// }}}

// vim:fdm=marker:
