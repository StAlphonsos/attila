#!/usr/bin/perl
##
# filename.cgi - purpose
#
# Time-stamp: <2007-03-20 19:40:14 attila@stalphonsos.com>
#
# Coyright (C) 1999-2007 by Sean Levy <snl@cluefactory.com>
# All Rights Reserved.
#
# See the POD at EOF for docs, or invoke with -help -verbose
##
use strict;
use warnings;
use Carp;
use CGI;

BEGIN {
    ($P) = reverse(split('/', $ENV{'SCRIPT_NAME'} || $::0)); # XXX File::Spec
    my $yyyy = 1900+(localtime(time))[5];
    $COPY_YEARS = sprintf(($yyyy == 2007) ? q{%d} : q{%d-%d}, 2007, $yyyy);
    $VERBOSE = 0;
    $DEFAULTS = {
    };
    $VERSION = '0.1.0';
}

##

MAIN: {
    my $cgi = CGI->new();
    ## ...
}

__END__

=pod

filename.cgi - cgi program brief descr

=head1 SYNOPSIS

  http://foo.bar/filename.cgi?param1=foo&...

=head1 DESCRIPTION

This is a CGI program, meant to be invoked by a web browser.

=head1 PARAMETERS

We accept the following parameters as GET or POST data.

=over 4

=item param1

Describe it...

=back

=head1 VERSION HISTORY

B<Alice>: Well I must say I've never heard it that way before...

B<Caterpillar>: I know, I have improved it. 

Z<>

  0.1.0   16 Feb 07     snl     Started

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
