##
# bmf.pl - flail/bmf integration
#
# Time-stamp: <2014-02-10 10:01:01 attila@stalphonsos.com>
##
use vars qw($BmfProgram);
$BmfProgram = '/usr/local/bin/bmf';

sub call_bmf {
    my($args,$folder,@range) = @_;
    if (!defined($FOLDER)) {
        flail_emit("No current folder.\n");
        return;
    } else {
        @range = ($FOLDER->current_message) unless @range;
        my @tmp;
        eval { @tmp = parse_range("@range",1); };
        if ($@) {
            warn("range expression bad (@range): $@\n");
            return;
        }
        @range = @tmp;
        my @move = ();
        my $count = 0;
        my $pos = 0;
        my $neg = 0;
        foreach my $msgno (@range) {
            my $msg = $FOLDER->get_message($msgno);
            if (!$msg) {
                warn("message $msgno does not exist - skipping\n") unless $Quiet;
            } else {
                open(BMF, "|$BmfProgram $args") or die(qq{could not invoke $BmfProgram $args on $msgno: $!\n});
                print BMF $msg->as_string();
                close(BMF);
                ++$count;
                my $bmf_status = $?;
                if ($bmf_status == 0) {
                    ++$pos;
                } else {
                    ++$neg;
                }
                if ($args eq '') {
                    my $head = $msg->head();
                    my $from = headaddr0($head,"From");
                    my($fauser,$fahost) = address_email($from);
                    $from = $fauser . '@' . $fahost;
                    my $subj = $head->get("Subject");
                    if (length($subj) > 16) {
                        $subj = substr($subj,0,16) . "...";
                    }
                    if ($bmf_status == 0) {
                        if ($::Verbose) {
                            flail_emit("[SPAM #$msgno <$from>: $subj]\n");
                        }
                        push(@move,$msgno);
                    } else {
                        if ($::Verbose) {
                            flail_emit("[NOT spam #$msgno <$from>: $subj]\n");
                        }
                    }
                } else {
                    push(@move,$msgno);
                }
            }
        }
        flail_emit("[Passed $count ($pos+$neg) msgs through: $BmfProgram $args]\n") unless $Quiet;
        flail_move(@move,$folder) if
            (!$::OPT->{'test'} && !$::OPT->{'check'} &&
             $folder && scalar(@move));
    }
}

sub cmd_bmf_filter { call_bmf("",spam_folder_name(),@_); }
sub cmd_bmf_spam { call_bmf("-s",spam_folder_name(),@_); }
sub cmd_bmf_notspam { call_bmf("-n",$IncomingFolder,@_); }
sub cmd_bmf_respam { call_bmf("-S",spam_folder_name(),@_); }
sub cmd_bmf_renotspam { call_bmf("-N",$IncomingFolder,@_); }
sub cmd_bmf_test { call_bmf("-t",undef,@_); }

sub cmd_bmf {
    my @args = @_;
    my $opt = "-s";
    if ($::OPT->{'filter'}) {
        $opt = "";
    }
    my $folder = latest_spam_folder();
    flail_emit("[This folder: ".$FOLDER->foldername()."]\n") unless $Quiet;
    if ($::OPT->{"re"}) {
        if ($::OPT->{"no"} || $::OPT->{"not"}) {
            $opt = "-N";
            $folder = $IncomingFolder;
        } else {
            $opt = "-S";
        }
    } elsif ($::OPT->{"no"} || $::OPT->{"not"}) {
        $opt = "-n";
        $folder = $IncomingFolder;
    } elsif ($::OPT->{"test"}) {
        $opt = "-t";
        $folder = undef;
    }
    $folder = undef
        if (defined($FOLDER) && ($folder eq $FOLDER->foldername()));
    call_bmf($opt,$folder,@args);
}

flail_defcmd1("spam",\&cmd_bmf,"bmf cmds: spam/no, spam/re, spam/no/re, spam/test (all w/noexec)");

flail_emit(" [BMF]") unless $Quiet;

1;

# Local variables:
# mode: perl
# indent-tabs-mode: nil
# tab-width: 4
# perl-indent-level: 4
# End:
