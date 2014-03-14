
/*
 * koar/nodes/fwriter.c
 * copyright (c) 2014 Frano Perleta
 */

#include <stdio.h>
#include <sndfile.h>
#include "buf.h"
#include "patchvm.h"
#include "nodes/active.h"

// state {{{

typedef struct fwriter_s* fwriter_t;

struct fwriter_s {
    SNDFILE* sf;
};

static void
fwriter_close (fwriter_t fw)
{
    if (fw->sf)
    {
        sf_close (fw->sf);
        fw->sf = NULL;
    }
}

// }}}

// callbacks {{{

static void
fwriter_exit (anode_t an)
{
    fwriter_close (anode_state (an));
}

static void
fwriter1_tick (patch_t p UNUSED, anode_t an, patch_stamp_t now, size_t delta)
{
    fwriter_t fw = anode_state (an);

    pnode_t src = anode_get_source (an, 0);
    buf_t b = { .p = pnode_read (src, now).p };

    if (fw->sf)
        sf_writef_float (fw->sf, b.p, delta);

    buf_release (b);
}

static void
fwriter2_tick (patch_t p UNUSED, anode_t an, patch_stamp_t now, size_t delta)
{
    fwriter_t fw = anode_state (an);

    pnode_t srcl = anode_get_source (an, 0);
    pnode_t srcr = anode_get_source (an, 1);
    buf_t bl = { .p = pnode_read (srcl, now).p };
    buf_t br = { .p = pnode_read (srcr, now).p };

    if (!(fw->sf))
    {
        buf_release (bl);
        buf_release (br);
        return;
    }

    samp_t buf[2*delta];
    size_t i;

    for (i = 0; i < delta; i++)
    {
        buf[2*i+0] = bl.xs[i];
        buf[2*i+1] = br.xs[i];
    }

    buf_release (bl);
    buf_release (br);

    if (fw->sf)
        sf_writef_float (fw->sf, buf, delta);
}

static struct ainfo_s
fwriter1_ainfo = {
    .name = "fwriter1",
    .init = NULL,
    .exit = fwriter_exit,
    .tick = fwriter1_tick,
    .ins = 1,
    .outs = 0,
    .size = sizeof (struct fwriter_s)
};

static struct ainfo_s
fwriter2_ainfo = {
    .name = "fwriter2",
    .init = NULL,
    .exit = fwriter_exit,
    .tick = fwriter2_tick,
    .ins = 2,
    .outs = 0,
    .size = sizeof (struct fwriter_s)
};

// }}}

// 1_make {{{

anode_t
N_fwriter1_make (patch_t p, const char* fn, size_t sr, pnode_t in)
{
    anode_t an = anode_create (p, &fwriter1_ainfo);
    anode_source (an, 0, in);

    fwriter_t fw = anode_state (an);

    SF_INFO sfi = {
        .samplerate = sr,
        .channels = 1,
        .format = SF_FORMAT_AIFF | SF_FORMAT_FLOAT
    };

    fw->sf = sf_open (fn, SFM_WRITE, &sfi);

    return an;
}

void
PATCHVM_fwriter1_make (patchvm_t vm, instr_t instr)
{
    patch_t p = patchvm_patch (vm);
    const char* fn = (const char*) instr->args[1].utf8;
    size_t sr = instr->args[2].nat;
    pnode_t in = patchvm_get (vm, instr->args[3].reg).pn;
    reg_t val = { .tag = T_FWRITER1, .an = N_fwriter1_make (p, fn, sr, in) };
    patchvm_set (vm, instr->args[0].reg, val);
}

// }}}

// 2_make {{{

anode_t
N_fwriter2_make (patch_t p, const char* fn, size_t sr, pnode_t inl, pnode_t inr)
{
    anode_t an = anode_create (p, &fwriter2_ainfo);
    anode_source (an, 0, inl);
    anode_source (an, 1, inr);

    fwriter_t fw = anode_state (an);

    SF_INFO sfi = {
        .samplerate = sr,
        .channels = 2,
        .format = SF_FORMAT_AIFF | SF_FORMAT_FLOAT
    };

    fw->sf = sf_open (fn, SFM_WRITE, &sfi);

    return an;
}

void
PATCHVM_fwriter2_make (patchvm_t vm, instr_t instr)
{
    patch_t p = patchvm_patch (vm);
    const char* fn = (const char*) instr->args[1].utf8;
    size_t sr = instr->args[2].nat;
    pnode_t inl = patchvm_get (vm, instr->args[3].reg).pn;
    pnode_t inr = patchvm_get (vm, instr->args[4].reg).pn;
    reg_t val = { .tag = T_FWRITER2, .an = N_fwriter2_make (p, fn, sr, inl, inr) };
    patchvm_set (vm, instr->args[0].reg, val);
}

// }}}

// close {{{

void
N_fwriter_close (patch_t p UNUSED, anode_t an)
{
    anode_lock (an);

    fwriter_t fw = anode_state (an);
    fwriter_close (fw);

    anode_unlock (an);
}

void
PATCHVM_fwriter_close (patchvm_t vm, instr_t instr)
{
    patch_t p = patchvm_patch (vm);
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    N_fwriter_close (p, an);
}

// }}}

// vim:fdm=marker:
