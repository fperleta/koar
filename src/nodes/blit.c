
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
    enum {
        UNIPOLAR = 0,
        BIPOLAR
    } mode;
    samp_t gain;
    samp_t phase;
};

// }}}

// processing loops {{{

static void __attribute__ ((hot))
blit_loop_up (blit_t blit, const samp_t* fs, samp_t* ys, size_t len)
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

        ys[i] = blit->gain * y;
        ph += f;
        ph -= floor (ph);
    }
    blit->phase = ph;
}

static void __attribute__ ((hot))
blit_loop_bp (blit_t blit, const samp_t* fs, samp_t* ys, size_t len)
{
    size_t i;

    samp_t ph = blit->phase;
    for (i = 0; i < len; i++)
    {
        samp_t f = fs[i];

        samp_t m = 2 * floor (0.5 / f);

        samp_t y;
        if (unlikely (ph < 0.0001))
            y = cos (m * ph * 2 * M_PI) / cos (ph * 2 * M_PI);
        else
            y = sin (m * ph * 2 * M_PI) / (m * sin (ph * 2 * M_PI));

        ys[i] = blit->gain * y;
        ph += 0.5 * f;
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

    switch (blit->mode)
    {
        case UNIPOLAR:
            blit_loop_up (blit, f.xs, b.xs, delta);
            break;
        case BIPOLAR:
            blit_loop_bp (blit, f.xs, b.xs, delta);
            break;
        default:
            panic ("this should never happen");
    }

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
    blit->mode = UNIPOLAR;
    blit->gain = 1;
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

// gain {{{

void
N_blit_gain (anode_t an, samp_t gain)
{
    blit_t blit = anode_state (an);
    blit->gain = gain;
}

void
PATCHVM_blit_gain (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    samp_t gain = instr->args[1].dbl;
    N_blit_gain (an, gain);
}

// }}}

// jump {{{

void
N_blit_jump (anode_t an, samp_t phase)
{
    blit_t blit = anode_state (an);
    blit->phase = phase - floor (phase);
}

void
PATCHVM_blit_jump (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    samp_t phase = instr->args[1].dbl;
    N_blit_jump (an, phase);
}

// }}}

// unipolar {{{

void
N_blit_unipolar (anode_t an)
{
    blit_t blit = anode_state (an);
    blit->mode = UNIPOLAR;
}

void
PATCHVM_blit_unipolar (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    N_blit_unipolar (an);
}

// }}}

// bipolar {{{

void
N_blit_bipolar (anode_t an)
{
    blit_t blit = anode_state (an);
    blit->mode = BIPOLAR;
}

void
PATCHVM_blit_bipolar (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    N_blit_bipolar (an);
}

// }}}

// vim:fdm=marker:
