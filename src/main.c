
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
#include "defs.h"

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
    va_list ap;

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
    if (!p)
        panic ("virtual memory exhausted");

    return p;
}

void*
xrealloc (void* p, size_t s)
{
    p = realloc (p, s);
    if (!p)
        panic ("virtual memory exhausted");

    return p;
}



const char* argp_program_version = "koar " KOAR_VERSION;

static struct argp_option opts[] = {
        { "daemon", 'd', 0, 0, "Detach from the terminal and run in the background.", 0 },
        { "quiet", 'q', 0, 0, "Don't print log messages to the standard output.", 0 },
        { "verbose", 'v', 0, 0, "Print all log messages. Repeat to enable logging.", 0 },
        { 0, 0, 0, 0, 0, 0 }
};

typedef struct {
    int daemon;
    int quiet;
    int verbose;
    int logging;
} args_t;

static const args_t default_args = {
    .daemon = 0,
    .quiet = 0,
    .verbose = 0,
    .logging = 0
};

static error_t
parse_opt (int key, char* arg UNUSED, struct argp_state* state)
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
        case 'v':
            a->logging = !!a->verbose;
            a->verbose = 1;
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

// echo {{{

static void
echo_on_message (peer_t* peer, proto_msg_t* msg)
{
    size_t len = msg->header.bytes;
    char buf[len + 1];
    memcpy (buf, msg->payload, len);
    buf[len] = 0;
    log_emit (LOG_NORMAL, "echo: %s", buf);
    peer_reply (peer, msg->header.mid, msg);
}

static void
echo_on_shutdown (peer_t* peer UNUSED)
{
    log_emit (LOG_NORMAL, "echo shutdown.");
}

peer_beh_t echo_beh = {
    echo_on_message,
    echo_on_shutdown
};

// }}}

static void
init (args_t* args)
{
    if (args->daemon)
    {
        log_emit (LOG_NORMAL, "detaching from the terminal...");
        daemon (1, 0);
    }

    DEBUGP ("%u", sizeof (proto_hdr_t));
    listener_create (&echo_beh, "tcp:20350");

    peer_t* peer = peer_connect (&echo_beh, "tcp:0.0.0.0:20350");
    proto_msg_t* msg = proto_msg_alloc (6);
    strcpy ((void*) msg->payload, "burek");
    peer_send (peer, msg);
}

static void
cleanup (void)
{
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

    init (&args);
    atexit (cleanup);

    /* main loop */ {
        struct ev_loop* loop = EV_DEFAULT;

        ev_signal sigint;
        ev_signal_init (&sigint, on_sigint, SIGINT);
        ev_signal_start (EV_A_ &sigint);

        ev_run (loop, 0);
    }

    exit (0);
}

