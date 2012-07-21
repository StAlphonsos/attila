##
# filename.pl - description
#
# Time-stamp: <2007-04-10 17:54:33 attila@stalphonsos.com>
##

use HTTP::Request;

sub cons_http_req {
    my $futs = qq|<?xml version="1.0"?><a>1</a>\n|;
    my $obj =
        HTTP::Request->new("GET","/foo",
                           [ "Content-type","application/xml",
                             "Content-length",length($futs) ],
                           $futs);
    $obj->protocol("HTTP/1.1");
    return $obj;
}

sub reck_http_req {
    my($buf) = @_;
    my($hdrs,$eol,$rest) = ($1,$2,$4)
        if ($buf =~ /^(.*?)(\n|\r\n)(\n|\r\n)(.*)$/s);
    warn("reck_http_req hdrs=|$hdrs| eol=|$eol| rest=|$rest|\n");
    return 0 unless ($hdrs && $eol);
    my @lines = split(/$eol/, $hdrs);
    warn("reck_http have ".scalar(@lines)." lines of hdrs\n");
    my $req = shift(@lines);
    my($method,$uri,$proto) = split(/\s+/,$req);
    warn("reck_http method=$method uri=$uri proto=$proto\n");
    return 0 unless ($method && ($method =~ /^(get|post|head)$/i) && $uri);
    my $hvec = [ map { split(/\s*:\s*/, $_, 2) } @lines ];
    warn("reck_http hvec=$hvec: ".join(", ", @$hvec)."\n");
    my $content = $rest;
    $buf = undef;
    for (my $i = 0; $i < scalar(@$hvec); $i += 2) {
        my($n,$v) = ($hvec->[$i],$hvec->[1+$i]);
        warn("reck_http hvec n=$n v=$v\n");
        if ($n =~ /content-length/i) {
            my $clen = $v;
            warn("reck_http content-length is $clen\n");
            my $nrest = length($rest);
            if ($nrest < $clen) {
                warn("reck_http Content-length says $clen, only have $nrest\n");
                return 0;
            } elsif ($nrest > $clen) {
                $content = substr($rest,0,$clen);
                $buf = substr($rest,$clen);
                last;
            }
        }
    }
    my $obj = HTTP::Request->new($method,$uri,$hvec,$content);
    $obj->protocol($proto);
    return($obj,$buf);
}

"usage:

    \$obj = cons_http_req();
    (\$obj,\$buf) = reck_http_req(\$buf);

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
