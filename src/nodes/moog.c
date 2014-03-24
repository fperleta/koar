
/*
 * koar/nodes/moog.c
 * copyright (c) 2014 Frano Perleta
 */

/*
 * A `moog` node models a Moog filter with variable cutoff frequency and
 * feedback.
 *
 * The implementation follows "An improved virtual analog model of the Moog
 * ladder filter" by Stefano D'Angelo and Vesa Välimäki.
 *
 * Internally, all the quantities are voltages in volts.
 *
 * Input signal:
 *
 *      V_in[n] = V_drive * x[n]
 *
 * Input to the first stage:
 *
 *                     V_in[n] + k[n] * V_out[n-1]
 *      V₀[n] = - tanh ---------------------------
 *                                2V_T
 *
 * The first stage:
 *
 *      dV₁[n] = A[n] * (V₀[n] - V₁[n-1])
 *
 *                        dV₁[n] + dV₁[n-1]
 *      Vp₁[n] = Vp₁[n] + -----------------
 *                              2f_s
 *
 *                   Vp₁[n]
 *      V₁[n] = tanh ------
 *                    2V_T
 *
 * Other stages are defined by substituting the appropriate indices into the
 * equations above.
 *
 * The output:
 *
 *      V_out[n] = Vp₄[n]
 *
 *      y[n] = V_out[n] / 2V_T
 *
 * The thermal voltage `V_T` is around 26mV at room temperature.
 * The sample rate `f_s` is 1.
 *
 * The cutoff frequency of the filter is controlled with the `A` parameter:
 *
 *                            1 - π f_c[n]
 *      A[n] = 4 V_T π f_c[n] ------------
 *                            1 + π f_c[n]
 *
 * The signals `f_c` (normalized cutoff frequency) and `k` (resonance ∈ [0, 4])
 * are drawn from the source pnodes of the `moog` sources.
 */

#include "buf.h"
#include "patchvm.h"
#include "nodes/active.h"

// state {{{

typedef struct moog_s* moog_t;

struct moog_s {
    samp_t gain;
    samp_t v_drive;
    samp_t v_thermal;
    // values from the previous iteration, effectively z^-1.
    samp_t v1, v2, v3, v4;
    samp_t dv1, dv2, dv3, dv4;
    samp_t vp1, vp2, vp3, vp4;
};

// }}}

// processing loop {{{

MACRO samp_t __attribute__ ((hot))
moog_sample (moog_t m, samp_t x, samp_t f, samp_t k)
{
    samp_t pif = M_PI * f;
    samp_t a = 4 * m->v_thermal * pif * (1 - pif) / (1 + pif);
    samp_t r2vt = 1 / (2 * m->v_thermal);

    samp_t v_in = m->v_drive * x;
    samp_t v0 = -tanh ((v_in + k * m->vp4) * r2vt);

    samp_t dv1 = a * (v0 - m->v1);
    samp_t vp1 = m->vp1 + 0.5 * (dv1 + m->dv1);
    samp_t v1 = tanh (vp1 * r2vt);
    m->dv1 = dv1; m->vp1 = vp1; m->v1 = v1;

    samp_t dv2 = a * (v1 - m->v2);
    samp_t vp2 = m->vp2 + 0.5 * (dv2 + m->dv2);
    samp_t v2 = tanh (vp2 * r2vt);
    m->dv2 = dv2; m->vp2 = vp2; m->v2 = v2;

    samp_t dv3 = a * (v2 - m->v3);
    samp_t vp3 = m->vp3 + 0.5 * (dv3 + m->dv3);
    samp_t v3 = tanh (vp3 * r2vt);
    m->dv3 = dv3; m->vp3 = vp3; m->v3 = v3;

    samp_t dv4 = a * (v3 - m->v4);
    samp_t vp4 = m->vp4 + 0.5 * (dv4 + m->dv4);
    samp_t v4 = tanh (vp4 * r2vt);
    m->dv4 = dv4; m->vp4 = vp4; m->v4 = v4;

    samp_t v_out = vp4 * r2vt;

#if 0
    printf ("v_in = %f; v0 = %f; a=%f; v1 = %f; v2 = %f; v3 = %f; v4 = %f; v_out = %f\n",
            v_in, v0, a, v1, v2, v3, v4, v_out);
#endif

    return v_out;
}

static void __attribute__ ((hot))
moog_loop (moog_t m, const samp_t* xs, const samp_t* fs, const samp_t* ks,
           samp_t* ys, size_t len)
{
    size_t i;

    for (i = 0; i < len; i++)
        ys[i] = m->gain * moog_sample (m, xs[i], fs[i], ks[i]);
}

// }}}

// callbacks {{{

static void
moog_tick (patch_t p, anode_t an, patch_stamp_t now, size_t delta)
{
    moog_t m = anode_state (an);

    pnode_t src = anode_get_source (an, 0);
    buf_t in = { .p = pnode_read (src, now).p };

    pnode_t fsig = anode_get_source (an, 1);
    buf_t freq = { .p = pnode_read (fsig, now).p };

    pnode_t ksig = anode_get_source (an, 2);
    buf_t res = { .p = pnode_read (ksig, now).p };

    buf_t b = buf_alloc (p->bufpool);

    moog_loop (m, in.xs, freq.xs, res.xs, b.xs, delta);

    buf_release (in);
    buf_release (freq);
    buf_release (res);

    pnode_t snk = anode_get_sink (an, 0);
    patch_datum_t out = { .b = b };
    pnode_write (p, snk, out, now);
}

static struct ainfo_s
moog_ainfo = {
    .name = "moog",
    .init = NULL,
    .exit = NULL,
    .tick = moog_tick,
    .ins = 3,
    .outs = 1,
    .size = sizeof (struct moog_s)
};

// }}}

// make {{{

anode_t
N_moog_make (patch_t p, pnode_t src, pnode_t fsig, pnode_t ksig, pnode_t snk)
{
    anode_t an = anode_create (p, &moog_ainfo);
    anode_source (an, 0, src);
    anode_source (an, 1, fsig);
    anode_source (an, 2, ksig);
    anode_sink (an, 0, snk);

    moog_t m = anode_state (an);
    m->gain = 1;
    m->v_drive = 0.0517;
    m->v_thermal = 25.85e-3;

    m->v1 = m->v2 = m->v3 = m->v4 = 0;
    m->dv1 = m->dv2 = m->dv3 = m->dv4 = 0;
    m->vp1 = m->vp2 = m->vp3 = m->vp4 = 0;

    return an;
}

void
PATCHVM_moog_make (patchvm_t vm, instr_t instr)
{
    patch_t p = patchvm_patch (vm);
    pnode_t src = patchvm_get (vm, instr->args[1].reg).pn;
    pnode_t fsig = patchvm_get (vm, instr->args[2].reg).pn;
    pnode_t ksig = patchvm_get (vm, instr->args[3].reg).pn;
    pnode_t snk = patchvm_get (vm, instr->args[4].reg).pn;
    reg_t val = { .tag = T_MOOG, .an = N_moog_make (p, src, fsig, ksig, snk) };
    patchvm_set (vm, instr->args[0].reg, val);
}

// }}}

// params {{{

void
N_moog_params (anode_t an, samp_t gain, samp_t drive, samp_t thermal)
{
    moog_t m = anode_state (an);
    m->gain = gain;
    m->v_drive = drive;
    m->v_thermal = thermal;
}

void
PATCHVM_moog_params (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    samp_t gain = instr->args[1].dbl;
    samp_t drive = instr->args[2].dbl;
    samp_t thermal = instr->args[3].dbl;
    N_moog_params (an, gain, drive, thermal);
}

// }}}

// vim:fdm=marker:
