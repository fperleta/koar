
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
    size_t offs;
    samp_t amp;
} tap_t;

typedef struct {
    size_t len, head;
    samp_t* xs;
    samp_t apc; // allpass coefficient
    samp_t out;

    samp_t damp_g, damp_p; // damping one-pole coeffs
} branch_t;

struct reverb_s {
    // tapped delay lines
    size_t tdl_len;
    size_t tdl_head;
    samp_t* tdl_ls; // these two are allocated in the same block.
    samp_t* tdl_rs;
    // ... the taps
    size_t tdl_ntaps;
    tap_t* tdl_ltap; // so are these.
    tap_t* tdl_rtap;
    // feedback delay network
    size_t fdn_nbranches;
    branch_t* fdn_branch;
    samp_t* fdn_fbs; // feedbacks -- all of these are in the same block:
    samp_t* fdn_ligs; // left TDL gains
    samp_t* fdn_rigs; // right TDL gains
    samp_t* fdn_logs; // left output gains
    samp_t* fdn_rogs; // right output gains
    // tonal correction filter
    samp_t tc_beta;
    samp_t tc_l, tc_r;
};

// }}}

// processing loop {{{

static samp_t __attribute__ ((hot))
tdl_sum (reverb_t rev, samp_t* xs, size_t hd, tap_t* ts)
{
    size_t i;
    samp_t acc = 0;

    size_t len = rev->tdl_len;
    for (i = 0; i < rev->tdl_ntaps; i++)
        if (ts[i].amp != 0)
        {
            size_t j = (len + hd - (ts[i].offs % len)) % len;
            acc += xs[j];
        }

    return acc;
}

static void
reverb_loop (reverb_t rev, const samp_t* lxs, const samp_t* rxs,
             samp_t* lys, samp_t* rys, size_t len)
{
    size_t i, j;
    size_t nbr = rev->fdn_nbranches;

    size_t hd = rev->tdl_head;
    for (i = 0; i < len; i++)
    {
        // push the inputs into the TDL.
        rev->tdl_ls[hd] = lxs[i];
        rev->tdl_rs[hd] = rxs[i];

        // early reflections.
        samp_t early_l = tdl_sum (rev, rev->tdl_ls, hd, rev->tdl_ltap);
        samp_t early_r = tdl_sum (rev, rev->tdl_rs, hd, rev->tdl_rtap);

        // advance the TDL head.
        hd = (hd + 1) % rev->tdl_len;

        // push inputs into the FDN
        samp_t outs[nbr];
        for (j = 0; j < nbr; j++)
        {
            branch_t* br = rev->fdn_branch + j;
            if (!(br->len))
            {
                outs[j] = 0;
                continue;
            }

            // accumulate the input into this branch.
            samp_t acc = early_l * rev->fdn_ligs[j] + early_r * rev->fdn_rigs[j];
            size_t k;
            for (k = 0; k < nbr; k++)
                acc += rev->fdn_fbs[k] * rev->fdn_branch[(j + k) % nbr].out;

            // update the delay line, schroeder allpass.
            samp_t old = br->xs[br->head];
            samp_t new = acc + br->apc * old;
            samp_t out1 = old - br->apc * new;
            br->xs[br->head] = new;
            br->head = (br->head + 1) % br->len;

            // damping.
            samp_t out2 = br->damp_g * out1 + br->damp_p * br->out;
            outs[j] = out2;
        }

        // pull outputs from the FDN
        samp_t fdn_l = 0, fdn_r = 0;
        for (j = 0; j < nbr; j++)
        {
            branch_t* br = rev->fdn_branch + j;
            br->out = outs[j];
            fdn_l += outs[j] * rev->fdn_logs[j];
            fdn_r += outs[j] * rev->fdn_rogs[j];
        }

        // tonal correction.
        samp_t out_l = (fdn_l - rev->tc_beta * rev->tc_l) / (1 - rev->tc_beta);
        samp_t out_r = (fdn_r - rev->tc_beta * rev->tc_r) / (1 - rev->tc_beta);
        rev->tc_l = out_l;
        rev->tc_r = out_r;

        // final output = early reflections + late reflections
        lys[i] = early_l + out_l;
        rys[i] = early_r + out_r;
    }
    rev->tdl_head = hd;
}

// }}}

// callbacks {{{

static void
reverb_exit (anode_t an)
{
    reverb_t rev = anode_state (an);

    if (rev->tdl_ls)
        free (rev->tdl_ls);
    rev->tdl_ls = rev->tdl_rs = NULL;

    if (rev->tdl_ntaps)
        free (rev->tdl_ltap);
    rev->tdl_ltap = rev->tdl_rtap = NULL;

    if (rev->fdn_fbs)
        free (rev->fdn_fbs);
    rev->fdn_fbs = rev->fdn_ligs = rev->fdn_rigs = NULL;
    rev->fdn_logs = rev->fdn_rogs = NULL;
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
               size_t tdl_len, size_t tdl_ntaps, size_t nbranches)
{
    anode_t an = anode_create (p, &reverb_ainfo);
    anode_source (an, 0, src1);
    anode_source (an, 1, src2);
    anode_sink (an, 0, snk1);
    anode_sink (an, 1, snk2);

    reverb_t rev = anode_state (an);
    rev->tdl_len = tdl_len;
    rev->tdl_head = 0;
    rev->tdl_ls = xmalloc (sizeof (samp_t) * 2 * tdl_len);
    rev->tdl_rs = rev->tdl_ls + tdl_len;
    rev->tdl_ntaps = tdl_ntaps;
    rev->tdl_ltap = xmalloc (sizeof (tap_t) * 2 * tdl_ntaps);
    rev->tdl_rtap = rev->tdl_ltap + tdl_ntaps;

    rev->fdn_nbranches = nbranches;
    rev->fdn_branch = xmalloc (sizeof (branch_t) * nbranches);
    rev->fdn_fbs = xmalloc (sizeof (samp_t) * 5 * nbranches);
    rev->fdn_ligs = rev->fdn_fbs + nbranches;
    rev->fdn_rigs = rev->fdn_fbs + 2 * nbranches;
    rev->fdn_logs = rev->fdn_fbs + 3 * nbranches;
    rev->fdn_rogs = rev->fdn_fbs + 4 * nbranches;

    rev->tc_beta = 0;
    rev->tc_l = rev->tc_r = 0;

    size_t i;
    for (i = 0; i < 2 * tdl_len; i++)
        rev->tdl_ls[i] = 0;
    for (i = 0; i < 2 * tdl_ntaps; i++)
        rev->tdl_ltap[i] = (tap_t) { .offs = 0, .amp = 0 };
    for (i = 0; i < nbranches; i++)
    {
        samp_t inbr = 1 / (samp_t) nbranches;
        rev->fdn_fbs[i] = inbr;
        rev->fdn_ligs[i] = inbr;
        rev->fdn_rigs[i] = inbr;
        rev->fdn_logs[i] = inbr;
        rev->fdn_rogs[i] = inbr;
        branch_t* br = rev->fdn_branch + i;
        br->len = br->head = 0;
        br->xs = NULL;
        br->apc = 0;
        br->out = 0;
        br->damp_g = 1;
        br->damp_p = 0;
    }

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
    size_t early_len = instr->args[5].nat;
    size_t early_count = instr->args[6].nat;
    size_t branches = instr->args[7].nat;
    reg_t val = { .tag = T_REVERB, .an = N_reverb_make (p, src1, src2, snk1, snk2, early_len, early_count, branches) };
    patchvm_set (vm, instr->args[0].reg, val);
}

// }}}

// early {{{

void
N_reverb_early (anode_t an, size_t index, size_t offs1, samp_t amp1, size_t offs2, samp_t amp2)
{
    reverb_t rev = anode_state (an);
    size_t i = index % rev->tdl_ntaps;
    rev->tdl_ltap[i] = (tap_t) { .offs = offs1, .amp = amp1 };
    rev->tdl_rtap[i] = (tap_t) { .offs = offs2, .amp = amp2 };
}

void
PATCHVM_reverb_early (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    size_t index = instr->args[1].nat;
    size_t offs1 = instr->args[2].nat;
    samp_t amp1 = instr->args[3].nat;
    size_t offs2 = instr->args[4].nat;
    samp_t amp2 = instr->args[5].nat;
    N_reverb_early (an, index, offs1, amp1, offs2, amp2);
}

// }}}

// branch {{{

void
N_reverb_branch (anode_t an, size_t index, size_t len, samp_t apc, samp_t damp_g, samp_t damp_p)
{
    reverb_t rev = anode_state (an);
    size_t i = index % rev->fdn_nbranches;
    branch_t* br = rev->fdn_branch + i;

    if (br->xs)
        free (br->xs);
    br->xs = xmalloc (sizeof (samp_t) * len);
    br->len = len;
    br->head = 0;
    br->apc = apc;
    br->out = 0;
    br->damp_g = damp_g;
    br->damp_p = damp_p;

    size_t j;
    for (j = 0; j < len; j++)
        br->xs[j] = 0;
}

void
PATCHVM_reverb_branch (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    size_t index = instr->args[1].nat;
    size_t len = instr->args[2].nat;
    samp_t apc = instr->args[3].dbl;
    samp_t damp_g = instr->args[4].dbl;
    samp_t damp_p = instr->args[5].dbl;
    N_reverb_branch (an, index, len, apc, damp_g, damp_p);
}

// }}}

// feedback {{{

void
N_reverb_feedback (anode_t an, size_t index, samp_t fb)
{
    reverb_t rev = anode_state (an);
    size_t i = index % rev->fdn_nbranches;
    rev->fdn_fbs[i] = fb;
}

void
PATCHVM_reverb_feedback (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    size_t index = instr->args[1].nat;
    samp_t fb = instr->args[2].dbl;
    N_reverb_feedback (an, index, fb);
}

// }}}

// gains {{{

void
N_reverb_gains (anode_t an, size_t index, samp_t lig, samp_t rig, samp_t log, samp_t rog)
{
    reverb_t rev = anode_state (an);
    size_t i = index % rev->fdn_nbranches;

    rev->fdn_ligs[i] = lig;
    rev->fdn_rigs[i] = rig;
    rev->fdn_logs[i] = log;
    rev->fdn_rogs[i] = rog;
}

void
PATCHVM_reverb_gains (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    size_t index = instr->args[1].nat;
    samp_t lig = instr->args[2].dbl;
    samp_t rig = instr->args[3].dbl;
    samp_t log = instr->args[4].dbl;
    samp_t rog = instr->args[5].dbl;
    N_reverb_branch (an, index, lig, rig, log, rog);
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
