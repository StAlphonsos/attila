#!/usr/bin/perl
##
# tmux.pl - tmux interface for flail
#
# Time-stamp: <2014-02-12 10:32:18 attila@stalphonsos.com>
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

package tmux;

sub new {
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
