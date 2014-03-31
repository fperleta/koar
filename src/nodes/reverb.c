
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
    size_t i;

    size_t hd = rev->tdl_head;
    for (i = 0; i < len; i++)
    {
        // push the inputs into the tdl.
        rev->tdl_ls[hd] = lxs[i];
        rev->tdl_rs[hd] = rxs[i];

        // early reflections.
        samp_t early_l = tdl_sum (rev, rev->tdl_ls, hd, rev->tdl_ltap);
        samp_t early_r = tdl_sum (rev, rev->tdl_rs, hd, rev->tdl_rtap);

        // advance the tdl head.
        hd = (hd + 1) % rev->tdl_len;

        // for now, just output the early reflections.
        lys[i] = early_l;
        rys[i] = early_r;
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
               size_t tdl_len, size_t tdl_ntaps)
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

    size_t i;
    for (i = 0; i < 2 * tdl_len; i++)
        rev->tdl_ls[i] = 0;
    for (i = 0; i < 2 * tdl_ntaps; i++)
        rev->tdl_ltap[i] = (tap_t) { .offs = 0, .amp = 0 };

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
    reg_t val = { .tag = T_REVERB, .an = N_reverb_make (p, src1, src2, snk1, snk2, early_len, early_count) };
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

// vim:fdm=marker:
