sub reduce {
    my($fn,@args) = @_;
    my $n = @args;
    return undef unless $n;
    return $args[0] if ($n == 1);
    return eval join($fn,@args) if !ref($fn); ## SO SKEEZY
    my($a0,@arest) = @args;
    return &$fn($a0,reduce($fn,@arest));
}

'usage:

        reduce(sub { my($a,$b)=@_; $a+$b; },1,2,3,4,5,6) => 21

';

__END__
