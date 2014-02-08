#! perl

use MIME::Base64;

sub extract_b64_parts {
    my($what) = @_;
    my @lines;
    if (ref($what) eq 'Mail::Internet') {
	@lines = @{$what->body};
    } elsif (ref($what) eq 'ARRAY') {
	@lines = @$what;
    } else {
	die("extract_b64_parts: what? $what\n");
    }
    my @parts;
    my $part = undef;
    foreach my $line (@lines) {
	if (!defined($part)) {
	    if (length($line) == 77) {
		$part = [ $line ];
	    } # else nothing
	} elsif (length($line) == 77) {
	    push(@$part, $line);
	} else {
	    push(@parts, $part);
	    $part = undef;
	}
    }
    return @parts;
}

sub decode_b64_parts {
    my($msg,$partno) = @_;
    $msg ||= $FOLDER->get_message($FOLDER->current_message);
    die(qq{decode_b64_parts: what message?\n}) unless $msg;
    my @parts = extract_b64_parts($msg);
    print "[Message has ".scalar(@parts)." base64 parts]\n";
    if (defined($partno)) {
	if (($partno < 0) || ($partno > $#parts)) {
	    warn("Message only has $#parts parts! partno=$partno\n");
	} else {
	    my $part = $parts[$partno];
	    my $decoded = decode_base64(join('',@$part));
	    print "Part $partno:\n";
	    print "=" x 60; print "\n";
	    print $decoded;
	    print "=" x 60; print "\n\n";
	}
    }
}

1;
