##
# filename.pl - description
#
# Time-stamp: <2009-05-22 20:12:41 attila@stalphonsos.com>
##

sub xencode {
    my($string) = @_;
    $string =~ s/([^[:print:]])/sprintf(q{&#%d;},ord($1))/egs;
    return $string;
}

"usage:

     \$string = xencode(\$string);

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
