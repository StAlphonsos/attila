##
# filename.pl - description
#
# Time-stamp: <2009-05-25 14:54:15 attila@stalphonsos.com>
##

sub revx { join('',reverse(unpack('(A2)*',$_[0]))); }

sub uuidmung {
    my($uuid) = @_;
    $uuid =~ s/-//gs;
    $uuid = uc($uuid);
    my($first8,$second4,$third4,$last16) = unpack('A8A4A4A16',$uuid);
    return revx($first8).revx($second4).revx($third4).$last16;
}

"usage:

     \$munged = uuidmung(\$string);

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
