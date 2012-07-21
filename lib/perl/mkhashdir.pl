##
# mkhashdir.pl - description
#
# Time-stamp: <2009-09-02 13:59:07 attila@stalphonsos.com>
##

sub dc {
    my($n,$l) = @_;
    if ($l == 0) { return 0 };
    if ($l == 1) { return $n; }
    return ($n*dc($n,$l-1))+$n;
}

sub mkhashdir_q {
    my($queue,$dir,$levels,$digits,$case,$alphabet) = @_;
    return $queue unless $levels > 0;
    $alphabet ||= "0123456789ABCDEF";
    my @list = ();
    my @dig = split('',$alphabet);
    while (@dig) {
        my @x = splice(@dig,0,$digits);
        my $nm = join('',@x);
        if ($case =~ /^u/) {
            $nm = uc($nm);
        } else {
            $nm = lc($nm);
        }
        my $path = "$dir/$nm";
        push(@list,$path);
    }
    push(@$queue,@list);
    mkhashdir_q($queue,$_,$levels-1,$digits,$case,$alphabet) foreach (@list);
    return $queue;
}

"usage:

     \$n = dc(\$dirs_per_level,\$n_levels);

     \$aref = mkhashdir_q(\$aref,\$dir,\$levels,\$digits,\$case);

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
