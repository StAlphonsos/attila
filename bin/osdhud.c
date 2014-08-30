/* -*- mode:c; c-basic-offset:4; tab-width:4; indent-tabs-mode:nil -*- */

/**
 * @file osdhud.c
 * @brief osd-based heads up display
 *
 * This command should be bound to some key in your window manager.
 * When invoked it brings up a heads-up display overlaid on the screen
 * via libosd.  It stays up for some configurable duration during
 * which it updates in real time, then disappears.  The default is for
 * the display to stay up for 2 seconds and update every 100
 * milliseconds.  The display includes load average, memory utilization,
 * swap utilization, network utilization, battery lifetime and uptime.
 */

/* LICENSE:
 *
 * Copyright (C) 2014 by attila <attila@haqistan.net>
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

#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/sysctl.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <vm/vm_param.h>
#include <sys/socket.h>
#include <net/if.h>
#include <net/if_mib.h>
#include <net/if_var.h>
#include <net/if_types.h>
#include <sys/un.h>
#include <xosd.h>

typedef struct osdhud_state {
    char               *argv0;
    int                 pid;
    char               *sock_path;
    char               *font;
    int                 pos_x;
    int                 pos_y;
    int                 width;
    int                 display_secs;
    int                 pause_msecs;
    int                 verbose;
    float               load_avg;
    float               net_kbps;
    float               mem_used_percent;
    float               swap_used_percent;
    int                 battery_life;
    int                 battery_life_avail;
    int                 battery_state;
    int                 battery_state_avail;
    int                 battery_time;
    int                 battery_time_avail;
    unsigned long       uptime_secs;
} osdhud_state_t;

#define DEFAULT_FONT "-adobe-helvetica-bold-r-normal-*-*-320-*-*-p-*-*-*"
#define DEFAULT_SOCK_PATH "/tmp/osdhud.sock"
#define DEFAULT_POS_X 10
#define DEFAULT_POS_Y 48
#define DEFAULT_WIDTH 50
#define DEFAULT_DISPLAY 2
#define DEFAULT_PAUSE 100

#define OSDHUD_STATE_INIT { .argv0=NULL, .pid=0, .sock_path=DEFAULT_SOCK_PATH, .font = DEFAULT_FONT, .pos_x = DEFAULT_POS_X, .pos_y = DEFAULT_POS_Y, .width = DEFAULT_WIDTH, .display_secs = DEFAULT_DISPLAY, .pause_msecs = DEFAULT_PAUSE, .verbose = 0, .load_avg = 0.0, .mem_used_percent = 0.0, .swap_used_percent = 0.0, .net_kbps = 0.0, .battery_life = 0,.battery_life_avail = 0, .battery_state = 0, .battery_state_avail = 0, .battery_time = 0, .battery_time_avail = 0, .uptime_secs = 0 }

#define DO_SYSCTL(dd,nn,mm,vv,zz,nn,nz) do { \
        if (sysctl(nn,mm,vv,zz,nn,nz)) { perror(dd); exit(1); } \
    } while (0);

static void probe_load(
    osdhud_state_t     *state)
{
    int mib[2] = { CTL_VM, VM_LOADAVG };
    struct loadavg avgs;
    size_t len = sizeof(avgs);

    DO_SYSCTL("vm.loadavg",mib,2,&avgs,&len,NULL,0);
    state->load_avg = (float)avgs.ldavg[0] / (float)avgs.fscale;
}

static void probe_mem(
    osdhud_state       *state)
{
    int mib[2] = { CTL_HW, HW_PAGESIZE };
    int pgsz = 0;
    size_t len = sizeof(pgsz);
    struct vmtotal vmtot;

    DO_SYSCTL("hw.pagesize",mib,2,&pgsz,&len,NULL,0);

    mib[0] = CTL_VM;
    mib[1] = VM_TOTAL;
    len = sizeof(vmtot);
    DO_SYSCTL("vm.total",mib,2,&vmtot,&len,NULL,0);

    state->mem_used_percent = (float)vmtot.t_avm / (float)vmtot.t_vm;
}

static void probe_swap(
    osdhud_state       *state)
{
}

static void probe_net(
    osdhud_state_t     *state)
{
    int mib[6] = {CTL_NET,PF_LINK,NETLINK_GENERIC,IFMIB_SYSTEM,IFMIB_IFCOUNT,0};
    int ifcount = 0;
    size_t len = sizeof(ifcount);
    int i = 0;

    DO_SYSCTL(mib,5,&ifcount,&len,NULL,0);
    mib[3] = IFMIB_IFDATA;
    mib[5] = IFDATA_GENERAL;
    for (i = 1; i < ifcount; i++) {
        struct ifmibdata ifmd;
        struct if_data *d = NULL;

        mib[4] = i;
        len = sizeof(ifmd);
        DO_SYSCTL("ifmib",mib,6,&ifmd,&len,NULL,0);
        if (!(ifmd.ifmd_flags & IFF_UP))
            continue;
        d = &ifmd.ifmd_data;
        printf("#%2d/%2d: %s flags=0x%x ipax=%lu ierr=%lu opax=%lu oerr=%lu recv=%lu sent=%lu\n",i,ifcount,ifmd.ifmd_name,ifmd.ifmd_flags,d->ifi_ipackets,d->ifi_ierrors,d->ifi_opackets,d->ifi_oerrors,d->ifi_ibytes,d->ifi_obytes);
    }
}

static void probe(
    osdhud_state_t     *state)
{
    probe_load(state);
    probe_mem(state);
    probe_swap(state);
    probe_net(state);
    probe_battery(state);
    probe_uptime(state);
}

static void display(
    osdhud_state_t     *state)
{
    display_load(state);
    display_mem(state);
    display_swap(state);
    display_net(state);
    display_battery(state);
    display_uptime(state);
}

static int pause(
    osdhud_state_t     *state)
{
}

static int kicked(
    osdhud_state_t     *state)
{
}

static void usage(
    osdhud_state_t     *state,
    char               *msg)
{
}

int main(
    int                 argc,
    char              **argv)
{
    int ch = 0;
    osdhud_state_t state = OSDHUD_STATE_INIT;

    state.argv0 = rindex(argv[0],'/');
    state.argv0 = state.argv0 ? state.argv0+1 : "?";
    while ((ch = getopt(argc,argv,"d:p:vf:h?")) != -1) {
        switch (ch) {
        case 'd':                       /* duration in seconds */
            if (sscanf(optarg,"%d",&state.display_secs) != 1)
                usage(&state,"bad value for -d");
            break;
        case 'p':                       /* inter-probe pause in milliseconds */
            if (sscanf(optarg,"%d",&state.pause_msecs) != 1)
                usage(&state,"bad value for -p");
            break;
        case 'v':                       /* verbose */
            state.verbose++;
            break;
        case 'f':                       /* font */
            state.font = optarg;
            break;
        case 's':                       /* path to unix socket */
            state.sock_path = optarg;
            break;
        case '?':                       /* help */
        case 'h':                       /* ditto */
            usage(&state,NULL);
            break;
        default:                        /* similar */
            usage(&state,"unknown option");
            break;
        }
    }
    if (kicked(&state)) {
        /* we were already running - just kick the existing process */
        if (state.verbose)
            printf("%s: kicked existing osdhud pid %d\n",state.argv0,state.pid);
    } else {
        /* not running - bring up the hud */
        int done = 0;

        do {
            probe(&state);
            done = pause(&state);
        } while (!done);
    }
    exit(0);
}
