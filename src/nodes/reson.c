
/*
 * koar/nodes/reson.c
 * copyright (c) 2014 Frano Perleta
 */

#include "buf.h"
#include "patchvm.h"
#include "nodes/active.h"

// state {{{

typedef struct reson_s* reson_t;

struct reson_s {
    enum {
        PURE_2POLE = 0,
        RES_GAIN,
        PEAK_GAIN,
        LOWPASS,
        HIGHPASS
    } mode;
    samp_t gain;
    samp_t h1, h2;
};

// }}}

// processing loop {{{

static void
reson_loop (reson_t rs, const samp_t* xs, const samp_t* fs, const samp_t* qs,
            samp_t* ys, size_t len)
{
    size_t i;

    samp_t h1 = rs->h1, h2 = rs->h2;
    switch (rs->mode)
    {
        case PURE_2POLE: // unnormalized 2-pole filter {{{
            for (i = 0; i < len; i++)
            {
                samp_t x = xs[i];
                samp_t y = x + h1;

                samp_t f = fs[i];
                samp_t r = 1 - M_PI * f / qs[i];
                samp_t a1 = -2 * r * cos (2*M_PI*f);
                samp_t a2 = r * r;

                h1 = -a1 * y + h2;
                h2 = -a2 * y;

                ys[i] = rs->gain * y;
            }
            break; // }}}

        case RES_GAIN: // constant resonance gain (zeros at ±R) {{{
            for (i = 0; i < len; i++)
            {
                samp_t x = xs[i];
                samp_t y = x + h1;

                samp_t f = fs[i];
                samp_t r = 1 - M_PI * f / qs[i];
                samp_t a1 = -2 * r * cos (2*M_PI*f);
                samp_t a2 = r * r;

                h1 = -a1 * y + h2;
                h2 = -r * x - a2 * y;

                ys[i] = rs->gain * (1 - r) * y;
            }
            break; // }}}

        case PEAK_GAIN: // constant peak gain (zeroes at ±1) {{{
            for (i = 0; i < len; i++)
            {
                samp_t x = xs[i];
                samp_t y = x + h1;

                samp_t f = fs[i];
                samp_t r = 1 - M_PI * f / qs[i];
                samp_t a1 = -2 * r * cos (2*M_PI*f);
                samp_t a2 = r * r;

                h1 = -a1 * y + h2;
                h2 = -x - a2 * y;

                ys[i] = rs->gain * 0.5 * (1 - r*r) * y;
            }
            break; // }}}

        case LOWPASS: // lowpass (both zeros at -1) {{{
            for (i = 0; i < len; i++)
            {
                samp_t x = xs[i];
                samp_t y = x + h1;

                samp_t f = fs[i];
                samp_t r = 1 - M_PI * f / qs[i];
                samp_t a1 = -2 * r * cos (2*M_PI*f);
                samp_t a2 = r * r;

                h1 = 2 * x - a1 * y + h2;
                h2 = x - a2 * y;

                // H(1) = 4/(1 + a1 + a2)
                ys[i] = rs->gain * (1 + a1 + a2) * 0.25 * y;
            }
            break; // }}}

        case HIGHPASS: // highpass (both zeros at 1) {{{
            for (i = 0; i < len; i++)
            {
                samp_t x = xs[i];
                samp_t y = x + h1;

                samp_t f = fs[i];
                samp_t r = 1 - M_PI * f / qs[i];
                samp_t a1 = -2 * r * cos (2*M_PI*f);
                samp_t a2 = r * r;

                h1 = -2 * x - a1 * y + h2;
                h2 = x - a2 * y;

                // H(-1) = 2/(1 - a1 + a2)
                ys[i] = rs->gain * (1 - a1 + a2) * 0.5 * y;
            }
            break; // }}}

        default:
            panic ("this should never happen");
    }
    rs->h1 = h1; rs->h2 = h2;

}

// }}}

// callbacks {{{

static void
reson_tick (patch_t p, anode_t an, patch_stamp_t now, size_t delta)
{
    reson_t rs = anode_state (an);

    pnode_t src = anode_get_source (an, 0);
    buf_t in = { .p = pnode_read (src, now).p };

    pnode_t fsig = anode_get_source (an, 1);
    buf_t f = { .p = pnode_read (fsig, now).p };

    pnode_t qsig = anode_get_source (an, 2);
    buf_t q = { .p = pnode_read (qsig, now).p };

    buf_t b = buf_alloc (p->bufpool);

    reson_loop (rs, in.xs, f.xs, q.xs, b.xs, delta);

    buf_release (in);
    buf_release (f);
    buf_release (q);

    pnode_t snk = anode_get_sink (an, 0);
    patch_datum_t out = { .b = b };
    pnode_write (p, snk, out, now);
}

static struct ainfo_s
reson_ainfo = {
    .name = "reson",
    .init = NULL,
    .exit = NULL,
    .tick = reson_tick,
    .ins = 3,
    .outs = 1,
    .size = sizeof (struct reson_s)
};

// }}}

// make {{{

anode_t
N_reson_make (patch_t p, pnode_t src, pnode_t fsig, pnode_t qsig, pnode_t snk)
{
    anode_t an = anode_create (p, &reson_ainfo);
    anode_source (an, 0, src);
    anode_source (an, 1, fsig);
    anode_source (an, 2, qsig);
    anode_sink (an, 0, snk);

    reson_t rs = anode_state (an);
    rs->mode = PURE_2POLE;
    rs->gain = 1;
    rs->h1 = rs->h2 = 0;

    return an;
}

void
PATCHVM_reson_make (patchvm_t vm, instr_t instr)
{
    patch_t p = patchvm_patch (vm);
    pnode_t src = patchvm_get (vm, instr->args[1].reg).pn;
    pnode_t fsig = patchvm_get (vm, instr->args[2].reg).pn;
    pnode_t qsig = patchvm_get (vm, instr->args[3].reg).pn;
    pnode_t snk = patchvm_get (vm, instr->args[4].reg).pn;
    reg_t val = { .tag = T_RESON, .an = N_reson_make (p, src, fsig, qsig, snk) };
    patchvm_set (vm, instr->args[0].reg, val);
}

// }}}

// pure {{{

void
N_reson_pure (anode_t an, samp_t gain)
{
    reson_t rs = anode_state (an);
    rs->mode = PURE_2POLE;
    rs->gain = gain;
}

void
PATCHVM_reson_pure (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    samp_t gain = instr->args[1].dbl;
    N_reson_pure (an, gain);
}

// }}}

// res {{{

void
N_reson_res (anode_t an, samp_t gain)
{
    reson_t rs = anode_state (an);
    rs->mode = RES_GAIN;
    rs->gain = gain;
}

void
PATCHVM_reson_res (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    samp_t gain = instr->args[1].dbl;
    N_reson_res (an, gain);
}

// }}}

// peak {{{

void
N_reson_peak (anode_t an, samp_t gain)
{
    reson_t rs = anode_state (an);
    rs->mode = PEAK_GAIN;
    rs->gain = gain;
}

void
PATCHVM_reson_peak (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    samp_t gain = instr->args[1].dbl;
    N_reson_peak (an, gain);
}

// }}}

// lowpass {{{

void
N_reson_lowpass (anode_t an, samp_t gain)
{
    reson_t rs = anode_state (an);
    rs->mode = LOWPASS;
    rs->gain = gain;
}

void
PATCHVM_reson_lowpass (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    samp_t gain = instr->args[1].dbl;
    N_reson_lowpass (an, gain);
}

// }}}

// highpass {{{

void
N_reson_highpass (anode_t an, samp_t gain)
{
    reson_t rs = anode_state (an);
    rs->mode = HIGHPASS;
    rs->gain = gain;
}

void
PATCHVM_reson_highpass (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    samp_t gain = instr->args[1].dbl;
    N_reson_highpass (an, gain);
}

// }}}

// vim:fdm=marker:
