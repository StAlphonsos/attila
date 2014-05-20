/* -*- mode:c; c-basic-offset:4; tab-width:4; indent-tabs-mode:nil -*- */

/**
 * @file dzenbar.c
 * @brief dzen2-compatible status bar
 *
 * Produce output suitable for use with dzen2 to give you configurable
 * status displays for any metric you can sample with a Unix command.
 */

/* LICENSE:
 *
 * Copyright (C) 1999-2014 by attila <attila@stalphonsos.com>
 *
 * Permission to use, copy, modify, and/or distribute this software for
 * any purpose with or without fee is hereby granted, provided that the
 * above copyright notice and this permission notice appear in all
 * copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
 * WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
 * AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
 * PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 */

#include <stdio.h>
#include <unistd.h>

extern char *optarg;
extern int opterr;
extern int optind;
extern int optopt;
extern int optreset;

#define OPTSTRING "vm:w:C:"

typedef struct gauge_state {
    float               max_val;
    float               min_val;
    int                 tot_width;
    float               val;
} gauge_state_t;

typedef struct our_state {
    int                 options;
} our_state_t;

#define OUR_STATE_INIT {.options=0}

static void cleanup_state(
    dzenbar_state_t    *state)
{
}

static void do_gadget(
    our_state_t        *state,
    char               *gadget)
{
    if (!strcmp(gadget,"load")) {
    } else if (!strcmp(gadget,"cpu")) {
    } else if (!strcmp(gadget,"netio")) {
    } else if (!strcmp(gadget,"temp")) {
    } else if (!strcmp(gadget,"mail")) {
    } else if (!strcmp(gadget,"clock")) {
    } else {
        usage();
    }
}

int main(
    int                 argc,
    char              **argv)
{
    int ch = -1;
    int done = 0;
    our_state_t state = OUR_STATE_INIT;

    while ((ch = getopt(argc, argv, OPTSTRING)) != -1) {
        switch (ch) {
        }
    }
    done = (state.options & DZB_LOOP)? 0: 1;
    do {
        for (int argno = optind; argno < argc; argno++)
            do_gadget(&state,argv[argno]);
        nloops++;
        if (!done)
            sleep(sleep_secs);
    } while (!done);
    exit(0);
}
