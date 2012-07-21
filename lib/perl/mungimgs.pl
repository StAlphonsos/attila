sub mungimgs {
	my($dir,$ext,$targ) = @_;
	my $x = lc($ext);
	$x = "jpeg" if $x eq 'jpg';
	my $tx = lc($targ);
	$tx = 'jpeg' if $tx eq 'jpg';
	my $filter = "pnmto${tx}";
	opendir(D,$dir) or die("$dir: $!\n");
	my @files = grep { $_ =~ /\.${ext}$/i } readdir(D);
	close(D);
	return map {
		my $munged = $_;
		$munged =~ s/\.${ext}$/.${targ}/i;
		my $fcmd = "${x}topnm < ${dir}/$_ | ${filter} > ${dir}/${munged}";
		qq|[ ! -f ${munged} ] && {\n\tls -l ${dir}/$_\n\t${fcmd}\n\tls -l ${dir}/${munged}\n}|;
	} @files;
}

sub mungscript {
	my($outfile,$dir,$ext,$targ) = @_;
	$dir ||= '.';
	$ext ||= 'JPG';
	$targ ||= 'jpg';
	my @cmds = mungimgs($dir,$ext,$targ);
	open(SCRIPT,"> $outfile") || die("$outfile: $!\n");
	print SCRIPT "#!/bin/sh\n";
	print SCRIPT join("\n",@cmds)."\n";
	close(SCRIPT);
	print "wrote ".scalar(@cmds)." commands => $outfile\n";
}

"usage:

	mungimgs('.','JPG','png') => list of commands

	mungscript('script.sh','.','JPG','png') => write script
"
__END__
