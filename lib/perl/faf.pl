##
# faf.pl - find_all_files
#
# Time-stamp: <2007-04-07 12:32:23 attila@stalphonsos.com>
##

sub find_all_files {
    my($subdir,$regexp) = @_;
    my @files = ();
    if (opendir(SUBDIR,$subdir)) {
        ## This is a little wasteful, but it's only the build system...
#+D         print STDERR "reading SUBDIR $subdir...\n";
        my @list = (
            grep {
                ($_ !~ /^[\.\#]/) && ($_ !~ /~$/) &&
                ((-d "$subdir/$_") || ($_ =~ /\.$regexp$/))
            } readdir(SUBDIR)
        );
#+D         print STDERR "read $subdir, $!\n";
        closedir(SUBDIR);
#+D         print STDERR "find_all_files($subdir,$regexp) raw list=(@list)\n";
        push(@files, map { "$subdir/$_" } grep { /\.$regexp$/ } @list);
        push(@files, find_all_files("$subdir/$_",$regexp))
            foreach (grep { -d "$subdir/$_" } @list);
#+D         print STDERR "find_all_files($subdir,$regexp) final list=(@list)\n";
    }
    return @files;
}

"usage:

    \@list_of_files = find_all_files(\$subdir,\$ext_regexp);

";

__END__
