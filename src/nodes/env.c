
/*
 * koar/nodes/env.c
 * copyright (c) 2014 Frano Perleta
 */

#include "buf.h"
#include "patchvm.h"
#include "nodes/env.h"

// state {{{

typedef enum {
    ENV_CONST = 0,
    ENV_LIN
} env_mode_t;

typedef struct env_s* env_t;

struct env_s {
    env_mode_t mode;
    samp_t x0;
    samp_t x1;
    double t;
};

// }}}

// callbacks {{{

static void
env_init (patch_t p UNUSED, anode_t an UNUSED)
{
}

static void
env_exit (anode_t an UNUSED)
{
}

static void
env_tick (patch_t p, anode_t an, patch_stamp_t now, size_t delta)
{
    env_t env = anode_state (an);
    buf_t b = buf_alloc (p->bufpool);

    switch (env->mode)
    {
        case ENV_CONST:
            buf_const (b, env->x0, delta);
            break;

        case ENV_LIN:
            {
                size_t t = floor (env->t);
                size_t n = (delta < t)? delta : t;
                samp_t d = env->x1 - env->x0;
                buf_lin (b, env->x0, d / t, n);
                env->x0 += d * n / t;
                env->t -= n;

                if (env->t < 1)
                {
                    env->mode = ENV_CONST;
                    env->x0 = env->x1;
                    env->t = 0;
                }
            }
            break;

        default:
            break;
    }

    patch_datum_t out = { .b = b };
    pnode_write (p, anode_get_sink (an, 0), out, now);
}

static struct ainfo_s
env_ainfo = {
    .init = env_init,
    .exit = env_exit,
    .tick = env_tick,
    .ins = 0,
    .outs = 1,
    .size = sizeof (struct env_s)
};

// }}}

// make {{{

anode_t
N_env_make (patch_t p, pnode_t out, samp_t x0)
{
    anode_t an = anode_create (p, &env_ainfo);
    anode_sink (an, 0, out);

    env_t env = anode_state (an);
    env->mode = ENV_CONST;
    env->x0 = env->x1 = x0;
    env->t = 0;

    return an;
}

void
PATCHVM_env_make (patchvm_t vm, instr_t instr)
{
    patch_t p = patchvm_patch (vm);
    pnode_t out = patchvm_get (vm, instr->args[1].reg).pn;
    double x0 = instr->args[2].dbl;
    reg_t val = { .tag = T_ENV, .an = N_env_make (p, out, x0) };
    patchvm_set (vm, instr->args[0].reg, val);
}

// }}}

// const {{{

void
N_env_const (anode_t an, samp_t x0)
{
    anode_lock (an);

    env_t env = anode_state (an);
    env->mode = ENV_CONST;
    env->x0 = env->x1 = x0;
    env->t = 0;

    anode_unlock (an);
}

void
PATCHVM_env_const (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    double x0 = instr->args[1].dbl;
    N_env_const (an, x0);
}

// }}}

// lin {{{

void
N_env_lin (anode_t an, samp_t x1, double t)
{
    anode_lock (an);

    env_t env = anode_state (an);
    env->mode = ENV_LIN;
    env->x1 = x1;
    env->t = t;

    anode_unlock (an);
}

void
PATCHVM_env_lin (patchvm_t vm, instr_t instr)
{
    anode_t an = patchvm_get (vm, instr->args[0].reg).an;
    double x1 = instr->args[1].dbl;
    double t = instr->args[2].dbl;
    N_env_lin (an, x1, t);
}

// }}}

// vim:fdm=marker:
