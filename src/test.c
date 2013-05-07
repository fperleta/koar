
#include <stdio.h>
#include "patch.h"
#include "defs.h"

static void
dummy_init (patch_t p UNUSED, anode_t an UNUSED)
{
}

static void
echo_tick (patch_t p UNUSED, anode_t an, patch_stamp_t now, size_t dt UNUSED)
{
    pnode_t pn = anode_get_input (an, 0);
    double d = pnode_read (pn, now).d;
    log_emit (LOG_NORMAL, "%zu: %lf", now, d);
}

static void
randgen_tick (patch_t p, anode_t an, patch_stamp_t now, size_t dt UNUSED)
{
    double d = (rand () & 0xFFFF) / 65536.0;
    pnode_t pn = anode_get_output (an, 0);
    pnode_write (p, pn, (patch_datum_t) d, now);
}

static void
dummy_msg (patch_t p UNUSED, anode_t an UNUSED, unsigned msg UNUSED)
{
}

struct ainfo_s echo = {
    .init = dummy_init,
    .tick = echo_tick,
    .msg = dummy_msg,
    .ins = 1,
    .outs = 0,
    .size = 0
};

struct ainfo_s randgen = {
    .init = dummy_init,
    .tick = randgen_tick,
    .msg = dummy_msg,
    .ins = 0,
    .outs = 1,
    .size = 0
};

static patch_datum_t
sum_combine (patch_datum_t a, patch_datum_t b)
{
    return (patch_datum_t) (a.d + b.d);
}

struct pinfo_s sum = {
    .neutral = (patch_datum_t) 0.0,
    .combine = sum_combine
};

void
test (void)
{
    patch_t p = patch_create (4);

    anode_t e = anode_create (p, &echo);
    anode_t r1 = anode_create (p, &randgen);
    anode_t r2 = anode_create (p, &randgen);

    pnode_t s = pnode_create (&sum);

    anode_source (e, 0, s);
    anode_sink (r1, 0, s);
    anode_sink (r2, 0, s);

    size_t i;
    for (i = 0; i < 1000000; i++)
    {
        patch_activate (p, r1);
        patch_activate (p, r2);

        patch_tick (p);
    }
    patch_destroy (p);

    exit (EXIT_SUCCESS);
}

