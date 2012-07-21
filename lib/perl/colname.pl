##
# filename.pl - description
#
# Time-stamp: <2010-09-24 16:37:35 attila@stalphonsos.com>
##

sub colname {
    my($col) = @_;
    my $nalpha = 1+(ord('Z')-ord('A'));
    my $n = int($col / $nalpha);
    my $m = $col % $nalpha;
    return chr(ord('A')+$col) unless $n;
    return chr(ord('A')+($n-1)) . chr(ord('A')+$m);
#    return chr(ord('A')+$col) if ($col < ord('Z'));
}

"usage:

     \$spreadsheet_colname = colname(\$integer_column_offset);

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
