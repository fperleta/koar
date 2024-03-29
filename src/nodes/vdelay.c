
/*
 * koar/nodes/vdelay.c
 * copyright (c) 2014 Frano Perleta
 */

#include "buf.h"
#include "patchvm.h"
#include "nodes/active.h"

// state {{{

typedef struct vdelay_s* vdelay_t;

struct vdelay_s {
    enum {
        DELAY_TIME = 0,
        DELAY_FREQ
    } mode;
    size_t len, head;
    samp_t* hs;
    samp_t s1, s2;
    samp_t g_raw, g_del, g_fb;
};

// }}}

// processing loop {{{

static void __attribute__ ((hot))
vd_loop (vdelay_t vd, const samp_t* xs, const samp_t* ds, samp_t* ys, size_t len)
{
    size_t i, n = vd->len, hd = vd->head;
    samp_t* hs = vd->hs;

    samp_t s1 = vd->s1, s2 = vd->s2;
    for (i = 0; i < len; i++)
    {
        samp_t x = xs[i];
        samp_t d = (vd->mode == DELAY_TIME)? fabs (ds[i]) : 1/fabs (ds[i]);

        int l = round (d - 2);
        size_t offs = likely (l > 0)? l : 0;
        samp_t dd = d - offs;

        // non-interpolating delay line
        samp_t v
            = unlikely (offs == 0)? x
            : unlikely (offs > n)? 0
            : hs[(n + hd - offs) % n];

        // second-order thiran allpass interpolator
        samp_t a1 = -(dd - 2) / (dd + 1);
        samp_t a2 = (dd - 2) * (dd - 1) / ((dd + 1) * (dd + 2));

        samp_t y = a2 * v + s1;
        s1 = a1 * v - a1 * y + s2;
        s2 = v - a2 * y;

        ys[i] = vd->g_raw * x + vd->g_del * y;
        hs[hd] = x + vd->g_fb * y;
        hd = (hd + 1) % n;
    }
    vd->s1 = s1;
    vd->s2 = s2;

    vd->head = hd;
}

// }}}

// callbacks {{{

static void
vdelay_exit (anode_t an)
{
    vdelay_t vd = anode_state (an);
    if (vd->hs)
        free (vd->hs);
    vd->hs = NULL;
}

static void
vdelay_tick (patch_t p, anode_t an, patch_stamp_t now, size_t delta)
{
    vdelay_t vd = anode_state (an);

    pnode_t src = anode_get_source (an, 0);
    buf_t in = { .p = pnode_read (src, now).p };

    pnode_t dsig = anode_get_source (an, 1);
    buf_t d = { .p = pnode_read (dsig, now).p };

    buf_t b = buf_alloc (p->bufpool);

    vd_loop (vd, in.xs, d.xs, b.xs, delta);

    buf_release (in);
    buf_release (d);

    pnode_t snk = anode_get_sink (an, 0);
    patch_datum_t out = { .b = b };
    pnode_write (p, snk, out, now);
}

static struct ainfo_s
vdelay_ainfo = {
    .name = "vdelay",
    .init = NULL,
    .exit = vdelay_exit,
    .tick = vdelay_tick,
    .ins = 2,
    .outs = 1,
    .size = sizeof (struct vdelay_s)
};

// }}}

// make {{{

anode_t
N_vdelay_make (patch_t p, pnode_t src, pnode_t dsig, pnode_t snk, size_t len)
{
    anode_t an = anode_create (p, &vdelay_ainfo);
    anode_source (an, 0, src);
    anode_source (an, 1, dsig);
    anode_sink (an, 0, snk);

    vdelay_t vd = anode_state (an);
    vd->len = len;
    vd->head = 0;
    vd->hs = xmalloc (sizeof (samp_t) * len);
    vd->s1 = vd->s2 = 0;
    vd->g_raw = 0;
    vd->g_del = 1;
    vd->g_fb = 0;

    /* initalize to zero */ {
        size_t i;
        for (i = 0; i < len; i++)
            vd->hs[i] = 0;
    }

    return an;
}

void
PATCHVM_vdelay_make (patchvm_t vm, instr_t instr)
{
    patch_t p = patchvm_patch (vm);
    pnode_t src = patchvm_get (vm, instr->args[1].reg).pn;
    pnode_t dsig = patchvm_get (vm, instr->args[2].reg).pn;
    pnode_t snk = patchvm_get (vm, instr->args[3].reg).pn;
    size_t len = instr->args[4].nat;
    reg_t val = { .tag = T_VDELAY,
                  .an = N_vdelay_make (p, src, dsig, snk, len) };
    patchvm_set (vm, instr->args[0].reg, val);
}

// }}}

// gains {{{

void
N_vdelay_gains (anode_t an, samp_t g_raw, samp_t g_del, samp_t g_fb)
{
    vdelay_t vd = anode_state (an);

    vd->g_raw = g_raw;
    vd->g_del = g_del;
    vd->g_fb = g_fb;
}

void
PATCHVM_vdelay_gains (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    samp_t g_raw = instr->args[1].dbl;
    samp_t g_del = instr->args[2].dbl;
    samp_t g_fb = instr->args[3].dbl;
    N_vdelay_gains (an, g_raw, g_del, g_fb);
}

// }}}

// freqmode {{{

void
N_vdelay_freqmode (anode_t an)
{
    vdelay_t vd = anode_state (an);
    vd->mode = DELAY_FREQ;
}

void
PATCHVM_vdelay_freqmode (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    N_vdelay_freqmode (an);
}

// }}}

// vim:fdm=marker:
