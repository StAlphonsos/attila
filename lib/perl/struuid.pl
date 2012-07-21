##
# filename.pl - description
#
# Time-stamp: <2010-01-04 19:37:42 attila@stalphonsos.com>
##

use UUID;

sub struuid {
    my($u) = @_;
    UUID::generate($u) unless $u;
    my $s = undef;
    UUID::unparse($u,$s);
    return $s;
}

"usage:

     \$string = struuid(); # generate UUID and return string

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
