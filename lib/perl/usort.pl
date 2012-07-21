##
# usort.pl - sort/uniq
#
# Time-stamp: <2007-04-05 13:45:35 attila@stalphonsos.com>
##

sub usort {
    my $sorter = sub { $a cmp $b };
    $sorter = shift(@_) if ref($_[0]) eq 'CODE';
    sort $sorter keys(%{{ map { $_ => 1 } @_ }});
}

"usage:

     usort(sub { \$b <=> \$a },2,1,2,4,5,6,5,6,4,9);
       => (9,6,5,4,2,1)

     usort(qw(abra cadabra hocus pokus alakazam));
       => (abra,alakazam,cadabra,hocus,pokus);

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
