#!/usr/bin/perl
##
# tmux.pl - tmux interface for flail
#
# Time-stamp: <2014-02-25 19:53:19 attila@stalphonsos.com>
#
# Coyright (C) 1999-2007 by Sean Levy <snl@cluefactory.com>
# All Rights Reserved.
#
# See the POD at EOF for docs, or invoke with -help -verbose
##
use strict;
use warnings;
use Pod::Usage;

=pod

tmux - perl tmux interface

=head1 SYNOPSIS

  use tmux;
  my $tmux = tmux->new();
  my $pane = $tmux->create_pane({...});
  $pane->select();
  $pane->display_message("foo");
  ...

=head1 DESCRIPTION

tmux api for perl that talks control-mode.

=cut

package tmux::thing;
use base qw(Exporter);

package tmux::session;
use base qw(tmux::thing);

oopackage tmux::window;
use base qw(tmux::thing);

package tmux::pane;
use base qw(tmux::thing);

package tmux::event;
use base qw(Exporter);

package tmux;
use strict;
use Carp;
require Exporter;
use vars qw($AUTOLOAD);
use base qw(Exporter);
use IPC::Open2;
use IO::Select;

sub _init {
    my $self = shift(@_);
    $self->{'opts'} ||= (@_ && ref($_[0]))? $_[0]: { @_ };
    my($tmux_out,$tmux_in);
    $self->{'pid'} = open2($tmux_out,$tmux_in,'tmux','-C');
    $self->{'out'} = $tmux_out;
    $self->{'in'} = $tmux_in;
    $self->{'tout'} = $self->{'opts'}->{'timeout'} || 1;
    $self->{'select'} = IO::Select->new();
    $self->{'select'}->add($tmux_out);
    $self->{'inbuf'} = '';
    $self->{'dying'} = 0;
    $self->{'objects'} = []
    my $junk = $self->_read();
    warn("_init_new: junk: $junk\n");
    $self->_parse();
}

sub _cleanup {
    my($self) = @_;
    if ($self->{'pid'}) {
        warn("tmux: reaping tmux -C pid ".$self->{pid});
        waitpid($self->{'pid'},0);
        $self->{'pid'} = undef;
    }
    if ($self->{'inbuf'}) {
        warn("tmux: restart pitching inbuf: |".$self->{inbuf}."|\n");
        $self->{'inbuf'} = '';
    }
}

sub restart {
    my($self) = @_;
    $self->_cleanup();
    $self->_init();
}

sub reset {
    my($self) = @_;
    my @objs = @{$self->{'objects'}};
    $self->{'objects'} = [];
    $self->_reset_parser();
    return @objs;
}

sub _read {
    my($self) = @_;
    my $restart_needed = 0;
    my @ready = $self->{'select'}->can_read($self->{'tout'});
    while (@ready && !$restart_needed) {
        foreach my $handle (@ready) {
            my $buf = '';
            my $nread = $handle->sysread($buf,1024);
            if (!$nread) {
                warn("tmux died: restarting connection");
                $restart_needed = 1;
            } else {
                $self->{'inbuf'} .= $buf;
            }
        }
        @ready = $self->{'select'}->can_read($self->{'tout'})
            unless $restart_needed;
    }
    my $result = $self->{'inbuf'};
    $self->_cleanup() if ($self->{'dying'} || $restart_needed);
    $self->_init() if $restart_needed;
    return $result;
}

sub _start_block {
}

sub _end_block {
}

sub _reset_parser {
}

sub _in_block {
}

sub _append_block {
}

sub _parse {
    my($self) = @_;
    my @lines = split(/\n/,$self->{'inbuf'});
    my @results = ();
    my $leftovers = '';
    $self->_reset_parser();
    foreach my $line (@lines) {
        warn("parsing line: $line\n");
        if ($self->_in_block()) {
            if ($line =~ /^%end\s+(\d+)\s+(\d+)$/) {
                push(@results,$self->_end_block($1,$2));
            } else {
                $self->_append_block($line);
            }
        } elsif ($line =~ /^%begin\s+(\d+)\s+(\d+)$/) {
            $self->_start_block($1,$2);
        } elsif ($line =~ /^%([-\w_]+)(|\s+.*)$/) {
            $self->_parse_event($1,$2);
        } else {
        }
    }
    $self->{'inbuf'} = $leftovers;
    return @results;
}

sub new {
    my $proto = shift(@_);
    my $class = ref($proto) || $proto;
    $proto = {} unless ref($proto);
    bless($proto,$class);
    $proto->_init(@_);
    return $proto;
}

sub finish {
    my($self) = @_;
    if ($self->{'pid'}) {
        $self->{'dying'} = 1;
        $self->{'in'}->write("\n");
        $self->{'in'}->flush();
        $self->_read();
    }
}

sub DESTROY {
}

sub _execute {
    my($self,$cmd,@args) = @_;
    my $cmdstr = "$cmd @args";
    warn("tmux<<< $cmdstr\n");
    $self->{'inbuf'} = '';
    $self->{'in'}->write("$cmdstr\n");
    $self->{'in'}->flush();
    $self->_read();
    my @objs = $self->_parse();
    push(@{$self->{'objects'}},@objs);
    return shift(@{$self->{'objects'}}) unless wantarray;
    return @{$self->{'objects'}};
}

sub AUTOLOAD {
    my $self = shift(@_);
    my $name = $AUTOLOAD;
    $name =~ s/^tmux:://;
    my $cmd = $name;
    $cmd =~ s/_/-/gs;
    warn("tmux: AUTOLOAD: $name -> $cmd @_\n");
    return $self->_execute($cmd,@_)
}

=pod

=head1 VERSION HISTORY

B<Alice>: Well I must say I've never heard it that way before...

B<Caterpillar>: I know, I have improved it. 

Z<>

  0.1.0   12 Feb 14     attila  Started

=cut

##
# Local variables:
# mode: perl
# tab-width: 4
# perl-indent-level: 4
# perl-continued-statement-offset: 4
# indent-tabs-mode: nil
# comment-column: 40
# End:
##
