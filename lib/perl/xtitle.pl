
sub xterm_topic {
    my($text) = @_;

    STDERR->autoflush(1);
    print STDERR "\033k$text\033\\";
    #print STDERR "\033]0;$text\007";
}

1;
