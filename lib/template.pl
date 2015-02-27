#!/usr/bin/perl
##
# filename.pl - brief purpose
#
# See the POD at EOF for documentation and license.
##
use strict;
use warnings;
use Pod::Usage;
use vars qw($P $COPY_YEARS $YEAR_WRITTEN $VERBOSE $DEFAULTS $VERSION $ALIASES);

##
## attila's ye olde scriptie boilerplate
##

BEGIN {
    ($P) = reverse(split('/',$0));
    my $yyyy = 1900+(localtime(time))[5];
    $YEAR_WRITTEN = 2015;
    $COPY_YEARS = sprintf(
        ($yyyy == $YEAR_WRITTEN) ? q{%d} : q{%d-%d}, $YEAR_WRITTEN, $yyyy
    );
    $VERBOSE = 0;
    $DEFAULTS = {
    };
    $ALIASES = {
        'v' => 'verbose',
    };
    $VERSION = '0.1.0';
}

# qchomp - trim leading and trailing whitespace and deal with quoted strings
#
sub qchomp {
    my $str = shift(@_);
    while ($str =~ /^\s*([\"\'])(.*)\1\s*$/) {
        $str = $2;
    }
    $str =~ s/^\s+//;
    $str =~ s/\s+$//;
    return $str;
}

# usage - dump a usage message and die
#
sub usage {
    my($msg) = @_;
    pod2usage(-verbose => 2)            if $VERBOSE && !defined($msg);
    if (defined($msg)) {
        print STDERR "$P: ERROR: $msg\n"     if defined $msg;
    } else {
        print STDERR "$P: purpose of $P\n";
    }
    print STDERR "usage: $P [-options] [args]\n";
    print STDERR "       Options:\n";
    print STDERR "          -v --verbose        increment verbosity level\n";
    print STDERR "          -h --help           print this brief message\n";
    print STDERR "       To see the full documentation, try:\n\n";
    print STDERR "           \$ $P --help --verbose\n";
    exit(defined($msg)? 1:0);
}

# parse_argv - simplistic and effective CLA parser
#
# We turn @ARGV into a hashref, with the options given as the keys.
#
# If called with no args we start with an empty hashref; otherwise our
# argument should be a hashref and it becomes the starting point for
# our final product.  Double-dash options can have values, e.g.
# --option=val; if none is given then the option is treated as an
# auto-increment/bool option.  Single-dash options are single-letter
# as per ancient Unix custom and are split if they contain multiple
# options, e.g. -vcd is shorthand for -v -c -d.
#
# The special 'option' name '_' (underscore) collects any non-option
# arguments into an arrayref; you cannot have an option named underscore.
#
# If we are given an argument then the keys and values in it will be
# starting points / defaults (overriding $DEFAULTS).  If options take
# multiple values (because they can be specified multiple times) then
# initialize then to arrayrefs, e.g.
#
#    my $args = parse_argv({ '_' => [], 'multiple-values' => [] });
#
# If the user specified
#
#    $ whatever --multiple-values=a --multiple-values=b
#
# you'll get back a hashref with 'multiple-values' => ['a','b']
#
# If instead a hashref is given as the value then the option's value
# will be parsed as key:val, so
#
#    $args = parse_argv({ 'property' => {} });
#
# invoked with
#
#    $ whatever --property=color:black --property=size:small
#
# will result in a hashref with
#
#    'property' => { 'color' => 'black', 'size' => 'small' }
#
# Otherwise options have scalar values.  If no value is given on the
# command line the value is assumed to be an int and is incremented.
# This means that invoking us like so will cause an error at runtime:
#
#    $ whatever --foo=bar --foo
#
# Since the first instance of --foo set its value to 'bar' and the
# second one will try to increment a string.
#
# After parsing @ARGV we apply any aliases in the global $ALIASES
# hashref, which defines aliases in the option namespace.  By default
# we have: 'v' => 'verbose', 'h' => 'help'.  After aliases, the
# $DEFAULTS hashref is applied, which maps option names to default
# values.  Finally we check for the special option name 'verbose' and
# set the global $VERBOSE to its value.  If the special option
# 'debug-options' is present we dump out the results of our work at
# the very end.
#
sub parse_argv {
    my $args;
    if (@_ && (ref($_[0]) eq 'HASH')) {
        $args = shift(@_);
    } else {
        $args = { '_' => [] };
    }
    my @argv = @_;
    foreach my $arg (@argv) {
        $arg =~ s/^\s+//;
        $arg =~ s/\s+$//;
        next unless length $arg;
        if ($arg =~ /^(--[^=]+?)[=](.*)$/) {
            my($k,$v) = ($1,qchomp($2));
            $k =~ s/^-+//;
            if ($k ne '_') {
                if (!exists($args->{$k}) ||
                    (ref($args->{$k}) !~ /^(ARRAY|HASH)$/)) {
                    $args->{$k} = $v;
                } elsif (ref($args->{$k}) eq 'HASH') {
                    my($kk,$vv) = split(/:/,$v,2);
                    $args->{$k}->{$kk} = $vv;
                } else {
                    push(@{$args->{$k}}, $v);
                }
            } else {
                $args->{$k} = [] unless defined $args->{$k};
                push(@{$args->{$k}}, $v);
            }
        } elsif ($arg =~ /^(--.*)$/) {
            my $k = qchomp($1);
            $k =~ s/^-+//;
            if ($k ne '_') {
                ++$args->{$k};
            } else {
                usage(qq{Cannot have an option named underscore});
            }
        } elsif ($arg =~ /^-([^\-]+)$/) {
            ++$args->{$_} foreach (grep { $_ ne '_' } split('',qchomp($1)));
        } else {
            $args->{'_'} = [] unless defined $args->{'_'};
            push(@{$args->{'_'}}, $arg);
        }
    }
    # Now apply option aliases
    foreach (keys(%$ALIASES)) {
        $args->{$ALIASES->{$_}} = $args->{$_}
            if (defined($args->{$_}) && !defined($args->{$ALIASES->{$_}}))
    }
    # Next apply defaults, so aliases can take effect
    foreach (keys(%$DEFAULTS)) {
        $args->{$_} = $DEFAULTS->{$_} unless defined($args->{$_});
    }
    # Finally some skulduggery
    $VERBOSE = $args->{'verbose'};
    # All done
    if (defined($args->{'debug-options'})) {
        use Data::Dumper;
        local $Data::Dumper::Terse = 1;
        local $Data::Dumper::Indent = 0;
        warn(sprintf("$P: DEBUG-OPTIONS: '%s' => %s\n",$_,Dumper($args->{$_})))
             foreach (sort(keys(%$args)));
    }
    return $args;
}

##
## Application Logic
##

MAIN: {
    my $args = parse_argv({'_' => []}, @ARGV);
    usage() if $args->{'help'};
    ## ... Make a Jazz noise here
    exit(0);
}

__END__

=pod

program - purpose

=head1 SYNOPSIS

  # clever comment
  $ example command

=head1 DESCRIPTION

This utility exfoliates the widgetrumps with prebuscuprescient
exuberance.

With the oil of Aphrodite and the dust of the grand wazoo.

You might not believe this little fella, but it'll cure your asthma,
too.

=head1 OPTIONS

We accept the following optionology:

=over 4

=item -verbose (or -v)

=item -quiet

Be quiet about everything but errors.

=item -help

Print a short usage message.  If you specify -verbose, you get this
man page.

=item -version

Print our version

=back

=head1 VERSION HISTORY

B<Alice>: Well I must say I've never heard it that way before...

B<Caterpillar>: I know, I have improved it. 

Z<>

  0.1.0   ?? Feb ??    attila   Started

=head1 LICENSE

Copyright (C) 2015 by attila <attila@stalphonsos.com>

Permission to use, copy, modify, and/or distribute this software for
any purpose with or without fee is hereby granted, provided that the
above copyright notice and this permission notice appear in all
copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.

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
