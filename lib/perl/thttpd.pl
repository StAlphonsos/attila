##
#!/usr/bin/perl
##
# thttpd.pl - Tiny HTTP daemon
#
# Time-stamp: <2009-04-07 14:49:35 attila@stalphonsos.com>
##

use Socket;
use IO::Socket::INET;
use HTTP::Request;
use HTTP::Response;

$::THTTPD_FORM = <<__FoRM__;
<html>
 <head><title>a form</title></head>
 <body>
  <form action="/" method="post">
   <p>Path: <input name="path" type="text" size="20" value=""></p>
   <p>Op: <input name="code" type="text" size="3" value="1"></p>
   <p><input type="submit" name="submit" value="go"></p>
  </form>
 </body>
</html>
__FoRM__

sub thttpd_sock {
    my($port) = @_;
    $port ||= 9892;
    my $ssock = IO::Socket::INET->new(LocalPort => $port, Listen => 5, Reuse => 1);
    die(qq{no sock at $port: $!\n}) unless $ssock;
    warn("server sock on port $port\n");
    return $ssock;
}

sub thttpd_wait {
    my($ssock) = @_;
    warn("thttpd waiting for connection...\n");
    my($cli,$pn) = $ssock->accept();
    $cli->autoflush(1);
    my($pp,$pa) = sockaddr_in($pn);
    my $remote = "$pa:$pp";
    warn("thttpd <== $remote on #".fileno($cli)."\n");
    return($cli,$remote);
}

sub thttpd_getreq2 {
    my($cli,$buf) = @_;
    $buf ||= '';
    my($hdrs,$eol,$rest) = ($1,$2,$4)
        if ($buf =~ /^(.*)(\n|\r\n)(\n|\r\n)(.*)$/s);
    while (!defined($hdrs)) {
        my $line = <$cli>;
        warn("thttpd_getreq2 read line |$line|\n");
        $buf .= $line;
        ($hdrs,$eol,$rest) = ($1,$2,$4)
            if ($buf =~ /^(.*)(\n|\r\n)(\n|\r\n)(.*)$/s);
    }
    warn("thttpd got at least headers, eol len=".length($eol)." rest=|$rest|\n");
    my $clen = $1 if ($hdrs =~ /\bcontent-length:\s*(\d+)$eol/i);
    if (defined($clen)) {
        warn("thttpd looking for content-length=$clen bytes, have ".length($rest)."\n");
        while (length($rest) < $clen) {
            $rest .= <$cli>;
        }
        $buf = substr($rest,$clen);
        $rest = substr($rest,0,$clen);
        warn("thttpd found content have ".length($rest)." left over\n");
    } else {
        warn("thttpd no content length in hdrs: |$hdrs|\n");
    }
    my $req = HTTP::Request->parse("$hdrs$eol$rest");
    if (!defined($req)) {
        warn("thttpd could not not parse buffered req |$hdrs$eol$rest| (buf=$buf)\n");
    }
    return($req,$buf);
}

sub thttpd_getreq {
    my($cli) = @_;
    my $buf = '';
    my $saw_hdrs = 0;
    my $clen = undef;
    my $content = '';
    my $leftover = undef;
    while (defined(my $line = <$cli>)) {
        $buf .= $line;
        if (!$saw_hdrs && ($line =~ /^[\r\n]+$/)) {
            $saw_hdrs = 1;
            $clen = $1 if $buf =~ /\bcontent-length:\s+(\d+)\b/gsi;
            if (!defined($clen)) {
                warn("thttpd got complete hdrs w/no Content-length\n");
                last;
            } else {
                warn("thttpd expecting $clen bytes of content...\n");
            }
        } elsif ($clen) {
            $content .= $line;
            my $actual = length($content);
            if ($actual >= $clen) {
                warn("thttpd got $actual (clen=$clen) bytes\n");
                $buf .= substr($content,0,$clen);
                $leftover = substr($content,$clen) if ($actual > $clen);
                last;
            }
        } else {
            warn("thttpd read header line: $line");
        }
    }
    warn("thttpd buffered request: |$buf| leftover: |$leftover|\n");
    my $req = HTTP::Request->parse($buf);
    if (!defined($req)) {
        warn("thttpd could not read buffered requests from buf\n");
    }
    return $req;
}

sub thttpd_quickie {
    my($server) = @_;
    $server ||= thttpd_sock();
    my($client,$remote) = thttpd_wait($server);
    my($req,$buf) = (undef,'');
    if (!defined($client)) {
        warn("thttpd: no client!?\n");
    } else {
        $req = thttpd_getreq($client);
        warn("thttpd: got request: $req |".$req->as_string."|\n");
    }
    return($req,$server);
}

sub thttpd_quickie2 {
    my($client) = @_;
    my $req = thttpd_getreq($client);
    if (!defined($req)) {
        warn("thttpd could not read request\n");
    } else {
        warn("thttpd got request: |".$req->as_string()."|\n");
        thttpd_sendform($client);
    }
}

sub thttpd_quickie3 {
    my($server) = @_;
    $server ||= thttpd_sock();
    my($client,$remote) = thttpd_wait($server);
    if (!defined($client)) {
        warn("thttpd no client!\n");
    } else {
        thttpd_quickie2($client);
    }
    return($server,$client,$remote);
}

sub thttpd_sendresp {
    my($client,$code,$data,$hdrs) = @_;
    $hdrs ||= {};
    my $ctype = $hdrs->{'Content-type'} || 'text/html';
    my $clen = length($data) if defined($data);
    $client->print("200 OK Fine\r\n");
    $client->print("Content-type: $ctype\r\n");
    $client->print("Content-length: $clen\r\n") if $clen;
    $client->print("\r\n$data");
    $client->flush();
}

sub thttpd_sendform {
    my($client) = @_;
    thttpd_sendresp($client,200,$::THTTPD_FORM);
}

"Usage:

    \$server = thttpd_sock(\$portno); # create server socket

    (\$client,\$remote) = thttpd_wait(\$server); # accept conn

    \$req = thttpd_getreq(\$client); # return HTTP::Request object

    (\$req,\$buf) = thttpd_getreq2(\$client,\$buf); # alternate req parse

    (\$req,\$server,\$buf) = thttpd_quickie(\$server);

    thttpd_quickie2(\$client); # get req and send back form

    (\$server,\$client,\$remote) = thttpd_quickie3(\$server);

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
