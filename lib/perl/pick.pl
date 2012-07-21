##
# pick.pl - pick somehing
##

sub pick {
    return $_[int(rand(@_))] unless (@_ && ref($_[0]));
    return pick(keys(%{$_[0]})) if (ref($_[0]) eq 'HASH');
    return pick(@{$_[0]}) if (ref($_[0]) eq 'ARRAY');
    return undef;
}

sub pick_not {
    my($not,@args) = @_;
    my $eq =
        ($not =~ /^\d+$/) ?
            sub { $_[0] == $_[1]; } :
            sub { $_[0] eq $_[1]; };
    my $choice = pick(@args);
    my $ntrips = 0;
    while (&$eq($not,$choice)) {
        die("pick_not can't find anything but $choice\n") if (++$ntrips > 100);
        $choice = pick(@args);
    }
    return $choice;
}

sub pick2 {
    my @pair = (pick(@_));
    push(@pair,pick_not($pair[0],@_));
    return @pair;
}

"usage:

    \$item = pick(...something...);

    \$item = pick_not(\$avoid,...something...);

    \@pair = pick2(...something...);
";
__END__
