
/*
 * koar/nodes/noise.c
 * copyright (c) 2014 Frano Perleta
 */

#include "buf.h"
#include "patchvm.h"
#include "defs.h"

// state {{{

typedef enum {
    NOISE_WHITE = 0,
    NOISE_PINK
} noise_mode_t;

#define PINK_ORDER 16

typedef struct noise_s* noise_t;

struct noise_s {
    noise_mode_t mode;
    uint64_t rand0a, rand0b, rand1a, rand1b;
    samp_t xs[PINK_ORDER];
    samp_t p;
    size_t k;
};

#define PRIME0A 169743212279
#define PRIME0B 746212782439
#define PRIME1A 124134215221
#define PRIME1B 928742784451
#define PRIMEXX 5487896875543

// init {{{
MACRO void
noise_init (noise_t noise, uint64_t seed)
{
    noise->rand0a = (seed * PRIME0A) ^ seed;
    noise->rand0b = ~(seed * PRIME0B) ^ seed;
    noise->rand1a = (seed * PRIME1A) ^ seed;
    noise->rand1b = ~(seed * PRIME1B) ^ seed;

    size_t i;
    for (i = 0; i < PINK_ORDER; i++)
        noise->xs[i] = 0;
    noise->p = 0;
    noise->k = 0;
}
// }}}

// rand64 {{{
MACRO uint64_t
noise_rand64 (noise_t noise)
{
    uint64_t rand0a = noise->rand0a;
    uint64_t rand0b = noise->rand0b;
    uint64_t rand1a = noise->rand1a;
    uint64_t rand1b = noise->rand1b;

    rand0a = ((rand0a >> 1) | (rand0a << 63)) ^ (rand0a | ((rand0a << 1) | (rand0a >> 63)));
    rand0b = ((rand0b >> 1) | (rand0b << 63)) ^ (rand0b | ((rand0b << 1) | (rand0b >> 63)));
    rand1a = ((rand1a >> 1) | (rand1a << 63)) ^ (rand1a | ((rand1a << 1) | (rand1a >> 63)));
    rand1b = ((rand1b >> 1) | (rand1b << 63)) ^ (rand1b | ((rand1b << 1) | (rand1b >> 63)));

    noise->rand0a = rand0a;
    noise->rand0b = rand0b;
    noise->rand1a = rand1a;
    noise->rand1b = rand1b;

    uint64_t r0 = rand0a ^ rand0b;
    uint64_t r1 = rand1a ^ rand1b;

    return (r0 + PRIMEXX) ^ ((r1 << 7) | (r1 >> 57));
}
// }}}

// randunit {{{
MACRO samp_t
noise_randunit (noise_t noise)
{
    union {
        uint32_t u32;
        samp_t f;
    } x;

    x.u32 = noise_rand64 (noise) & 0xFFFFFFFF;
    x.u32 = (x.u32 & 0x007FFFFF) | 0x3F800000;

    return x.f - 1.0;
}
// }}}

// randsymm {{{
MACRO samp_t
noise_randsymm (noise_t noise)
{
    union {
        uint32_t u32;
        samp_t f;
    } x;

    x.u32 = noise_rand64 (noise) & 0xFFFFFFFF;
    x.u32 = (x.u32 & 0x007FFFFF) | 0x3F800000;

    return 2 * (x.f - 1.5);
}
// }}}

// }}}

// callbacks {{{

static void
noise_tick (patch_t p, anode_t an, patch_stamp_t now, size_t delta)
{
    noise_t noise = anode_state (an);
    buf_t b = buf_alloc (p->bufpool);
    size_t i;

    switch (noise->mode)
    {
        case NOISE_WHITE: // {{{
            for (i = 0; i < delta; i++)
                b.xs[i] = noise_randsymm (noise);
            break; // }}}

        case NOISE_PINK: // {{{
            {
                size_t k = noise->k;
                samp_t p = noise->p;
                for (i = 0; i < delta; i++)
                {
                    size_t j = __builtin_ctz (k + i) % PINK_ORDER;
                    samp_t delta = noise_randsymm (noise);
                    p -= noise->xs[j];
                    noise->xs[j] = delta;
                    p += delta;

                    b.xs[i] = (p + noise_randsymm (noise)) / 17;
                }
                noise->k = (k + delta) & ((1u << PINK_ORDER) - 1);
                noise->p = p;
            }
            break; // }}}

        default:
            break;
    }

    patch_datum_t x = { .b = b };
    pnode_write (p, anode_get_sink (an, 0), x, now);
}

static struct ainfo_s
noise_ainfo = {
    .name = "noise",
    .init = NULL,
    .exit = NULL,
    .tick = noise_tick,
    .ins = 0,
    .outs = 1,
    .size = sizeof (struct noise_s)
};

// }}}

// make {{{

anode_t
N_noise_make (patch_t p, pnode_t snk, unsigned seed)
{
    anode_t an = anode_create (p, &(noise_ainfo));
    anode_sink (an, 0, snk);

    noise_t noise = anode_state (an);
    noise->mode = NOISE_WHITE;
    noise_init (noise, seed);

    return an;
}

void
PATCHVM_noise_make (patchvm_t vm, instr_t instr)
{
    patch_t p = patchvm_patch (vm);
    pnode_t snk = patchvm_get (vm, instr->args[1].reg).pn;
    unsigned seed = instr->args[2].nat;
    reg_t val = { .tag = T_NOISE, .an = N_noise_make (p, snk, seed) };
    patchvm_set (vm, instr->args[0].reg, val);
}

// }}}

// seed {{{

void
N_noise_seed (anode_t an, unsigned seed)
{
    anode_lock (an);
    noise_t noise = anode_state (an);
    noise_init (noise, seed);
    anode_unlock (an);
}

void
PATCHVM_noise_seed (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    unsigned seed = instr->args[1].nat;
    N_noise_seed (an, seed);
}

// }}}

// white {{{

void
N_noise_white (anode_t an)
{
    anode_lock (an);
    noise_t noise = anode_state (an);
    noise->mode = NOISE_WHITE;
    anode_unlock (an);
}

void
PATCHVM_noise_white (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    N_noise_white (an);
}

// }}}

// pink {{{

void
N_noise_pink (anode_t an)
{
    anode_lock (an);
    noise_t noise = anode_state (an);
    noise->mode = NOISE_PINK;
    anode_unlock (an);
}

void
PATCHVM_noise_pink (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    N_noise_pink (an);
}


// }}}

// vim:fdm=marker:
