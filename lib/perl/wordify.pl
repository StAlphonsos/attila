##
# wordify.pl - reasonable command-line wordification
#
# Time-stamp: <2009-04-07 15:37:17 attila@stalphonsos.com>
##
use Text::Balanced qw(extract_delimited);

sub word_extract {
    my($string) = @_;
    my($x,$str) = extract_delimited($string,q{\"\'},'','\\');
    if ($x) {
        $x = substr($x,1,length($x)-2);
        $str =~ s/^\s+//;
    } elsif ($string =~ /^(\S+)\s+(\S.*)$/) {
        ($x,$str) = ($1,$2);
    } else {
        ($x,$str) = ($string,'');
    }
    return($x,$str);
}

sub wordify {
    my($string) = @_;
    $string =~ s/(^\s+|\s+$)//gs;
    my @words = ();
    my($word,$rest) = word_extract($string);
    while ($word) {
        push(@words, $word);
        ($word,$rest) = word_extract($rest);
    }
    return @words;
}

"usage:

   # uses Text::Balanced::extract_delimted:

   \(\$word,\$rest) = word_extract(\$string);

   \@words = wordify(\$string);

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
