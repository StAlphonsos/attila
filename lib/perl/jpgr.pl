sub mungimgs {
	my($dir,$ext,$targ,$filter) = @_;
	opendir(D,$dir) or die("$dir: $!\n");
	my @files = grep { $_ =~ /\.${ext}$/i } readdir(D);
	close(D);
	return map { my $pnm = $_; $pnm =~ 
}

"usage:

	jpgr('.','JPG','png') => list of commands
"
__END__