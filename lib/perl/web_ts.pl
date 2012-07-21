##
# web_ts.pl - time-stamp in standard web format, w/workaround for perl bug
#
# Time-stamp: <2007-04-11 09:22:47 attila@stalphonsos.com>
##
use POSIX;

sub web_ts {
    my($t) = @_;
    $t = CORE::time unless defined($t);
    my $ts = POSIX::strftime(q{%a, %d %b %Y %H:%M:%S %Z},gmtime($t));
    ## work-around Perl bug
    my $tz = POSIX::strftime(q{%Z},localtime($t));
    $ts =~ s/$tz/GMT/;
    return $ts;
}

"usage:

     \$ts_string = web_ts(\$optional_time_tt);

";

__END__

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
