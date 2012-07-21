#!/usr/bin/perl
##
use strict;
use IO::Handle;

MAIN: {
    my $text = "@ARGV";
    my $env = $ENV{'SCREEN_NAME'};
    $text = "<$env> $text" if $env;
    STDERR->autoflush(1);
    print STDERR "\033k$text\033\\";
    #print STDERR "\033]0;$text\007";
    exit(0);
}

1;
