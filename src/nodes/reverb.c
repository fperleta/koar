
/*
 * koar/nodes/reverb.c
 * copyright (c) 2014 Frano Perleta
 */

#include "buf.h"
#include "patchvm.h"
#include "nodes/active.h"

// state {{{

typedef struct reverb_s* reverb_t;

typedef struct {
    samp_t* xs;
} dline_t;

typedef struct {
    size_t offs;

    samp_t g, p; // one-pole coeffs

    samp_t z_1; // previous outputs
} dtap_t;

struct reverb_s {
    size_t nwalls;

    // delay lines:
    size_t wall_len, wall_head;
    size_t sink_len, sink_head;
    samp_t* samps_buf;
    dline_t* lines_buf;
    dline_t* walls;
    dline_t* lsink;
    dline_t* rsink;

    // dtaps:
    dtap_t* taps_buf;
    dtap_t* litaps; // nwalls
    dtap_t* ritaps;
    dtap_t* lotaps;
    dtap_t* rotaps;
    dtap_t* wwtaps; // nwalls*nwalls

    // tonal correction filter:
    samp_t tc_beta;
    samp_t tc_lz_1, tc_rz_1; // previous outputs
};

// }}}

// processing loop {{{

MACRO void __attribute__ ((hot))
dtap_write (dtap_t* tap, samp_t* xs, size_t hd, size_t len, samp_t x)
{
    samp_t y = tap->g * x + tap->p * tap->z_1;
    tap->z_1 = y;
    xs[(hd + tap->offs) % len] += y;
}

static void
reverb_loop (reverb_t rev, const samp_t* lxs, const samp_t* rxs,
             samp_t* lys, samp_t* rys, size_t len)
{
    size_t i, j, k;

    const size_t nwalls = rev->nwalls;
    const size_t wlen = rev->wall_len, slen = rev->sink_len;

    size_t whd = rev->wall_head, shd = rev->sink_head;
    for (i = 0; i < len; i++)
    {
        samp_t lx = lxs[i];
        samp_t rx = rxs[i];

        // sources to walls
        for (j = 0; j < nwalls; j++)
        {
            dtap_write (rev->litaps + j, rev->walls[j].xs, whd, wlen, lx);
            dtap_write (rev->ritaps + j, rev->walls[j].xs, whd, wlen, rx);
        }

        // walls to walls and sinks
        for (j = 0; j < nwalls; j++)
        {
            samp_t wx = -rev->walls[j].xs[whd];
            rev->walls[j].xs[whd] = 0;

            for (k = 0; k < j; k++)
                dtap_write (rev->wwtaps + j * nwalls + k, rev->walls[k].xs, whd, wlen, wx);
            for (++k; k < nwalls; k++)
                dtap_write (rev->wwtaps + j * nwalls + k, rev->walls[k].xs, whd, wlen, wx);

            dtap_write (rev->lotaps + j, rev->lsink->xs, shd, slen, wx);
            dtap_write (rev->rotaps + j, rev->rsink->xs, shd, slen, wx);
        }

        // outputs
        samp_t lo = rev->lsink->xs[shd];
        samp_t ro = rev->rsink->xs[shd];
        rev->lsink->xs[shd] = 0;
        rev->rsink->xs[shd] = 0;

        // tonal correction
        samp_t ly = (lo - rev->tc_beta * rev->tc_lz_1) / (1 - rev->tc_beta);
        samp_t ry = (ro - rev->tc_beta * rev->tc_rz_1) / (1 - rev->tc_beta);
        rev->tc_lz_1 = ly;
        rev->tc_rz_1 = ry;

        // step forward
        whd = (whd + 1) % wlen;
        shd = (shd + 1) % slen;

        lys[i] = ly;
        rys[i] = ry;
    }
    rev->wall_head = whd;
    rev->sink_head = shd;
}

// }}}

// callbacks {{{

static void
reverb_exit (anode_t an)
{
    reverb_t rev = anode_state (an);

    if (rev->samps_buf)
        free (rev->samps_buf);
    rev->samps_buf = NULL;

    if (rev->lines_buf)
        free (rev->lines_buf);
    rev->lines_buf = NULL;

    if (rev->taps_buf)
        free (rev->taps_buf);
    rev->taps_buf = NULL;
}

static void
reverb_tick (patch_t p, anode_t an, patch_stamp_t now, size_t delta)
{
    reverb_t rev = anode_state (an);

    pnode_t src1 = anode_get_source (an, 0);
    pnode_t src2 = anode_get_source (an, 1);
    buf_t in1 = { .p = pnode_read (src1, now).p };
    buf_t in2 = { .p = pnode_read (src2, now).p };

    buf_t b1 = buf_alloc (p->bufpool);
    buf_t b2 = buf_alloc (p->bufpool);

    reverb_loop (rev, in1.xs, in2.xs, b1.xs, b2.xs, delta);

    buf_release (in1);
    buf_release (in2);

    pnode_t snk1 = anode_get_sink (an, 0);
    pnode_t snk2 = anode_get_sink (an, 1);
    patch_datum_t out1 = { .b = b1 };
    patch_datum_t out2 = { .b = b2 };
    pnode_write (p, snk1, out1, now);
    pnode_write (p, snk2, out2, now);
}

static struct ainfo_s
reverb_ainfo = {
    .name = "reverb",
    .init = NULL,
    .exit = reverb_exit,
    .tick = reverb_tick,
    .ins = 2,
    .outs = 2,
    .size = sizeof (struct reverb_s)
};

// }}}

// make {{{

anode_t
N_reverb_make (patch_t p, pnode_t src1, pnode_t src2, pnode_t snk1, pnode_t snk2,
               size_t nwalls, size_t wall_len, size_t sink_len)
{
    anode_t an = anode_create (p, &reverb_ainfo);
    anode_source (an, 0, src1);
    anode_source (an, 1, src2);
    anode_sink (an, 0, snk1);
    anode_sink (an, 1, snk2);

    reverb_t rev = anode_state (an);

    rev->nwalls = nwalls;

    do { // delay lines {{{
        rev->wall_len = wall_len; rev->wall_head = 0;
        rev->sink_len = sink_len; rev->sink_head = 0;

        size_t nsamps = nwalls * wall_len + 2 * sink_len;
        samp_t* buf = rev->samps_buf = xmalloc (sizeof (samp_t) * nsamps);

        size_t i;
        for (i = 0; i < nsamps; i++)
            buf[i] = 0;

        dline_t* ls = xmalloc (sizeof (dline_t) * (nwalls + 2));
        rev->lines_buf = rev->walls = ls;
        rev->lsink = ls + nwalls;
        rev->rsink = ls + nwalls + 1;

        size_t j;
        for (j = 0; j < nwalls; j++)
        {
            dline_t* dl = ls + j;
            dl->xs = buf;
            buf += wall_len;
        }
        rev->lsink->xs = buf;
        rev->rsink->xs = buf + sink_len;
    } while (0); // }}}

    do { // taps {{{
        size_t ntaps = (4 + nwalls) * nwalls;
        dtap_t* buf = xmalloc (sizeof (dtap_t) * ntaps);

        rev->taps_buf = buf;
        rev->litaps = buf;
        rev->ritaps = buf + 1 * nwalls;
        rev->lotaps = buf + 2 * nwalls;
        rev->rotaps = buf + 3 * nwalls;
        rev->wwtaps = buf + 4 * nwalls;

        size_t i;
        for (i = 0; i < ntaps; i++)
        {
            dtap_t* tap = buf + i;
            tap->offs = 0;
            tap->g = 1;
            tap->p = 0;
            tap->z_1 = 0;
        }
    } while (0); // }}}

    do { // tonal correction filter {{{
        rev->tc_beta = 0;
        rev->tc_lz_1 = 0;
        rev->tc_rz_1 = 0;
    } while (0); // }}}

    return an;
}

void
PATCHVM_reverb_make (patchvm_t vm, instr_t instr)
{
    patch_t p = patchvm_patch (vm);
    pnode_t src1 = patchvm_get (vm, instr->args[1].reg).pn;
    pnode_t src2 = patchvm_get (vm, instr->args[2].reg).pn;
    pnode_t snk1 = patchvm_get (vm, instr->args[3].reg).pn;
    pnode_t snk2 = patchvm_get (vm, instr->args[4].reg).pn;
    size_t nwalls = instr->args[5].nat;
    size_t wall_len = instr->args[6].nat;
    size_t sink_len = instr->args[7].nat;
    reg_t val = { .tag = T_REVERB, .an = N_reverb_make (p, src1, src2, snk1, snk2, nwalls, wall_len, sink_len) };
    patchvm_set (vm, instr->args[0].reg, val);
}

// }}}

// internal {{{

void
N_reverb_internal (anode_t an, size_t w1, size_t w2, size_t offs,
                   samp_t g, samp_t p)
{
    reverb_t rev = anode_state (an);
    size_t i1 = w1 % rev->nwalls;
    size_t i2 = w2 % rev->nwalls;
    dtap_t* t = rev->wwtaps + i1 * rev->nwalls + i2;
    t->offs = offs; t->g = g; t->p = p;
}

void
PATCHVM_reverb_internal (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    size_t w1 = instr->args[1].nat;
    size_t w2 = instr->args[2].nat;
    size_t offs = instr->args[3].nat;
    samp_t g = instr->args[4].dbl;
    samp_t p = instr->args[5].dbl;
    N_reverb_internal (an, w1, w2, offs, g, p);
}

// }}}

// sources {{{

void
N_reverb_sources (anode_t an, size_t w,
                  size_t loffs, samp_t lg, samp_t lp,
                  size_t roffs, samp_t rg, samp_t rp)
{
    reverb_t rev = anode_state (an);
    size_t i = w % rev->nwalls;
    dtap_t* t;

    t = rev->litaps + i;
    t->offs = loffs; t->g = lg; t->p = lp;

    t = rev->ritaps + i;
    t->offs = roffs; t->g = rg; t->p = rp;
}

void
PATCHVM_reverb_sources (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    size_t w = instr->args[1].nat;
    size_t loffs = instr->args[2].nat;
    samp_t lg = instr->args[3].dbl;
    samp_t lp = instr->args[4].dbl;
    size_t roffs = instr->args[5].nat;
    samp_t rg = instr->args[6].dbl;
    samp_t rp = instr->args[7].dbl;
    N_reverb_sources (an, w, loffs, lg, lp, roffs, rg, rp);
}

// }}}

// sinks {{{

void
N_reverb_sinks (anode_t an, size_t w,
                size_t loffs, samp_t lg, samp_t lp,
                size_t roffs, samp_t rg, samp_t rp)
{
    reverb_t rev = anode_state (an);
    size_t i = w % rev->nwalls;
    dtap_t* t;

    t = rev->lotaps + i;
    t->offs = loffs; t->g = lg; t->p = lp;

    t = rev->rotaps + i;
    t->offs = roffs; t->g = rg; t->p = rp;
}

void
PATCHVM_reverb_sinks (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    size_t w = instr->args[1].nat;
    size_t loffs = instr->args[2].nat;
    samp_t lg = instr->args[3].dbl;
    samp_t lp = instr->args[4].dbl;
    size_t roffs = instr->args[5].nat;
    samp_t rg = instr->args[6].dbl;
    samp_t rp = instr->args[7].dbl;
    N_reverb_sinks (an, w, loffs, lg, lp, roffs, rg, rp);
}

// }}}

// tcfilter {{{

void
N_reverb_tcfilter (anode_t an, samp_t beta)
{
    reverb_t rev = anode_state (an);
    rev->tc_beta = beta;
}

void
PATCHVM_reverb_tcfilter (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    samp_t beta = instr->args[1].dbl;
    N_reverb_tcfilter (an, beta);
}

// }}}

// vim:fdm=marker:
