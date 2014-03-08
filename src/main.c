
/*
 * koar/main.c
 * copyright (c) 2013 Frano Perleta
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <time.h>
#include <argp.h>
#include <ev.h>
#include "peers.h"
#include "patchctl.h"
#include "patchvm.h"
#include "defs.h"

// logging {{{

static FILE* log_file = NULL;
static int log_stdout = 1;
static int log_verbose = 0;

static void close_log (void)
{
    if (log_file)
        fclose (log_file);
}

static const char* log_levels[] = {
    "!!",
    "--",
    "  ",
    "%%"
};

void
__log_emit (const char* fn, int ln, log_level_t lvl, const char* fmt, ...)
{
    char buf[1024];
    time_t t;
    char ts[64];
    va_list ap = {{0}};

    va_start (ap, fmt);
    vsnprintf (buf, 1024, fmt, ap);
    va_end (ap);

    if (log_file)
    {
        t = time (NULL);
        strftime (ts, 63, "(%H:%M:%S)", localtime (&t));
        fprintf (log_file, "%s (%s) %s:%d: %s\n", ts, log_levels[lvl], fn, ln, buf);
    }

    if (log_stdout)
        if (log_verbose || (lvl <= LOG_NORMAL))
            printf ("(%s) %s\n", log_levels[lvl], buf);
}

// }}}

// helper functions {{{

void __attribute__ ((noreturn))
__panic (void)
{
    ev_break (EV_DEFAULT_ EVBREAK_ALL);
    exit (EXIT_FAILURE);
}

void*
xmalloc (size_t s)
{
    void* p;

    p = malloc (s);
    if (!p && s)
        panic ("virtual memory exhausted");

    return p;
}

void*
xrealloc (void* p, size_t s)
{
    p = realloc (p, s);
    if (!p && s)
        panic ("virtual memory exhausted");

    return p;
}

// }}}

// command-line arguments {{{

const char* argp_program_version = "koar " KOAR_VERSION;

static struct argp_option opts[] = {
        { "daemon", 'd', 0, 0, "Detach from the terminal and run in the background.", 0 },
        { "quiet", 'q', 0, 0, "Don't print log messages to the standard output.", 0 },
        { "slave", 's', 0, 0, "Run as a slave process, reading patchvm bytecode from stdin.", 0},
        { "verbose", 'v', 0, 0, "Print all log messages. Repeat to enable logging.", 0 },
        { "workers", 'w', "N", 0, "Set the number of patch worker threads.", 0},
        { 0, 0, 0, 0, 0, 0 }
};

typedef struct {
    int daemon;
    int quiet;
    int verbose;
    int logging;
    int slave;
    size_t workers;
} args_t;

static const args_t default_args = {
    .daemon = 0,
    .quiet = 0,
    .verbose = 0,
    .logging = 0,
    .slave = 0,
    .workers = 1
};

static error_t
parse_opt (int key, char* arg, struct argp_state* state)
{
    args_t* a = state->input;
    switch (key)
    {
        case 'd':
            a->daemon = 1;
            break;
        case 'q':
            a->quiet = 1;
            break;
        case 's':
            a->slave = 1;
            break;
        case 'v':
            a->logging = !!a->verbose;
            a->verbose = 1;
            break;
        case 'w':
            a->workers = atoi (arg);
            break;
        default:
            return ARGP_ERR_UNKNOWN;
    }
    return 0;
}

static struct argp argp = {
    .options = opts,
    .parser = parse_opt,
    .args_doc = "",
    .doc = NULL
};

// }}}

static patchctl_endpoint_t ep = NULL;

static void
init (args_t* args)
{
    if (args->daemon)
    {
        log_emit (LOG_NORMAL, "detaching from the terminal...");
        daemon (1, 0);
    }

    if (!args->slave)
    {
        ep = patchctl_endpoint_create (EV_DEFAULT, "tcp:20350", 8, args->workers);
        if (!ep)
            panic ("unable to create a patchctl endpoint");
    }
}

static void
cleanup (void)
{
    if (ep)
        patchctl_endpoint_destroy (ep);
}

static void
on_sigint (struct ev_loop* loop, ev_signal* sig UNUSED, int revents UNUSED)
{
    log_emit (LOG_NORMAL, "received SIGINT, exiting...");
    ev_break (loop, EVBREAK_ALL);
}

int
main (int argc, char** argv)
{
    args_t args = default_args;
    argp_parse (&argp, argc, argv, 0, 0, &args);

    if (args.daemon || args.quiet)
        log_stdout = 0;
    log_verbose = args.verbose;

    log_emit (LOG_NORMAL, "koar %s - copyright (c) 2013 Frano Perleta", KOAR_VERSION);

    /* open the log file in /tmp */
    if (args.logging)
    {
        char buf[1024];

        snprintf (buf, 1023, "/tmp/koar.log.%d", getpid ());

        log_emit (LOG_DETAIL, "writing the log to %s", buf);

        log_file = fopen (buf, "a");
        atexit (close_log);
    }

    atexit (cleanup);
    init (&args);

    if (args.slave) /* slave mode */
    {
        patchvm_t vm = patchvm_create (args.workers, 0);

        if (patchvm_file (vm, stdin) != 0)
            exit (EXIT_FAILURE);
    }
    else /* main loop */
    {
        struct ev_loop* loop = EV_DEFAULT;

        ev_signal sigint;
        ev_signal_init (&sigint, on_sigint, SIGINT);
        ev_signal_start (EV_A_ &sigint);

        ev_run (loop, 0);
    }

    exit (EXIT_SUCCESS);
}

// vim:fdm=marker
