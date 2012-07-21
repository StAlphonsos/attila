# -*- mode:perl; indent-tabs-mode:nil; tab-width:2; perl-indent-level:2 -*-
#
# Utils.pm - Sean's bag of perl.trix
#
# Copyright (C) 2001 by Carnegie Mellon University.  All Rights Reserved.
# Copyright (C) 1997-2001 by Sean Levy.  All Rights Reserved.
#
# Time-stamp: <2004-03-30 16:50:57 EST>
# $Id: Utils.pm,v 1.1 2005/11/07 17:30:00 attila Exp $
#
# A few needful things taken from my toolchest.
#
package Utils;
use strict;
no strict 'refs';
BEGIN {
  use IO::File;
  use POSIX qw(strftime mktime floor);
  use Time::HiRes qw(usleep);           # _loglock - disgusting, only to debug
  use Fcntl qw(:flock);
  require Exporter;
  use vars
    qw(%MONTH_DAYS %DAYS_OF_WEEK $VERSION @ISA @EXPORT @EXPORT_OK $DEBUGGING
       %YEARS %MONTH_NUMS %DAY_NUMS $LOGSTAMP_FMT %EXPORT_TAGS $TESTING
       $LOGSTAMP_SHORT $LOGLEVEL $LOGPREF $_LOG_LOCK $_LOG_LOCK_SLEEP
       $_LOG_LOCK_MAX_TRIES %MONTH_NAMES
      );
  $VERSION = '0.2.1';
  @ISA = qw(Exporter);
  ## Exportable names
  @EXPORT_OK = qw(&saylog &plural &elapsed_string &antivenom &antivenom_lite
                  &exclude &parse_db_ts &dbdie &get_monday &is_leapyear
                  &logstamp &month_days &antivenom_filename &nl2br
                  &htmlspecialchars &countchar &initprog &is_printchar
                  %MONTH_DAYS %DAYS_OF_WEEK $LOGSTAMP_FMT
                  &errmsg &debuglog &testlog &modlog &errlog
                  &xml_walk &reach &reduce &gen_db_ts
                 );
  ## Names exported by default
  @EXPORT = qw(&saylog &antivenom &antivenom_lite
               &parse_db_ts &dbdie &antivenom_filename &nl2br &gen_db_ts
               &htmlspecialchars
               &countchar &is_printchar
               &errmsg &debuglog &testlog &modlog &errlog &reduce
              );
  ## Specially named subsets of exported names:
  ##   use Utils qw/:date/     - date functions
  ##   use Utils qw/:php/      - php compatibility
  ##   use Utils qw/:log/      - logging funcitons
  ##   use Utils qw/:web/      - web programming utilities
  ##   use Utils qw/:db/       - database utilities
  ##   use Utils qw/:string/   - string utilities
  ##   use Utils qw/:general/  - a general selection of stuff
  ##   use Utils qw/:all/      - everything
  %EXPORT_TAGS = ( date => [ qw!&get_monday &is_leapyear &month_days
                                %MONTH_DAYS %DAYS_OF_WEEK! ],
                   php => [ qw!&nl2br &htmlspecialchars! ],
                   log => [ qw!&saylog &logstamp $LOGSTAMP_FMT! ],
                   web => [qw!&antivenom &antivenom_lite
                              &antivenom_filename! ],
                   db => [ qw!&dbdie &parse_db_ts &gen_db_ts! ],
                   string => [ qw!&countchar &plural &elapsed_string
                                  &is_printchar! ],
                   general => [ qw!&exclude &saylog &antivenom &elapsed_string
                                   &plural &countchar &dbdie &parse_db_ts
                                   &reduce! ],
                   log => [ qw!&logstamp &errmsg &debuglog &testlog &modlog
                               &saylog &errlog! ],
                   xml => [ qw!&xml_walk &reach! ],
                   all => \@EXPORT_OK
                 );
  ## Data we define
  %MONTH_DAYS = ( 1 => 31,  3 => 31,  4 => 30,  5 => 31,
                  6 => 30,  7 => 31,  8 => 31,  9 => 30,
                 10 => 31, 11 => 30, 12 => 31
                );
  %DAYS_OF_WEEK = ( 0 => "Sunday",    1 => "Monday",   2 => "Tuesday",
                    3 => "Wednesday", 4 => "Thursday", 5 => "Friday",
                    6 => "Saturday"
                  );
  ## These seemingly nonsensical hashes make more sense when you look at
  ## HTMLStream::display_2array() and the date input widget thingies in that
  ## module.                                                            --S
  %YEARS = ( 2001 => 2001, 2002 => 2002, 2003 => 2003, 2004 => 2004,
             2005 => 2005, 2006 => 2006, 2007 => 2007, 2008 => 2008,
             2009 => 2009, 2010 => 2010, 2011 => 2011, 2012 => 2012,
             2013 => 2013, 2014 => 2014, 2015 => 2015, 2016 => 2016,
             2017 => 2017, 2018 => 2018, 2019 => 2019, 2020 => 2020
           );
  %MONTH_NUMS = (  1 =>  1,  2 =>  2,  3 =>  3,  4 =>  4,  5 =>  5,  6 =>  6,
                   7 =>  7,  8 =>  8,  9 =>  9, 10 => 10, 11 => 11, 12 => 12
                );
  %MONTH_NAMES = ( jan => 1, feb => 2, mar => 3, apr => 4, may => 5, jun => 6,
                   jul => 7, aug => 8, sep => 9, oct =>10, nov =>11, dec =>12
                 );
  %DAY_NUMS = ( 1=> 1, 2=> 2, 3=> 3, 4=> 4, 5=> 5, 6=> 6, 7=> 7, 8=> 8, 9=> 9,
               10=>10,11=>11,12=>12,13=>13,14=>14,15=>15,16=>16,17=>17,18=>18,
               19=>19,20=>20,21=>21,22=>22,23=>23,24=>24,25=>25,26=>26,27=>27,
               28=>28,29=>29,30=>30,31=>31
              );
  ## Non debugare est status quo.
  $LOGLEVEL = 9;
  $LOGPREF = '[Utils.pm] ';
  $DEBUGGING = 0;
  $TESTING = 0;
  ## I like this format myself, but let's leave it configurable.
  $LOGSTAMP_FMT = defined($main::LOGSTAMP_FMT)? $main::LOGSTAMP_FMT:
    "%Y-%m-%d-%H:%M:%S";
  $LOGSTAMP_SHORT = defined($main::LOGSTAMP_SHORT)? $main::LOGSTAMP_SHORT: 0;
  $_LOG_LOCK = defined($main::_LOG_LOCK)? $main::_LOG_LOCK: undef;
  $_LOG_LOCK = $ENV{LOG_LOCK} unless $_LOG_LOCK;
  $_LOG_LOCK = $ENV{HOME}."/._log_lock" unless $_LOG_LOCK;
  $_LOG_LOCK_MAX_TRIES = 5;
  $_LOG_LOCK_SLEEP = 500000;
  ##
  sub logstamp;
  sub _loglock {
    my $diddle = shift(@_);
    my $fn = shift(@_) || $_LOG_LOCK;
    my $tries = 0;
    if (!$diddle) {
      flock(LL, LOCK_UN);
      close(LL);
    } else {
      while ($tries < $_LOG_LOCK_MAX_TRIES) {
        my $failed = undef;
        open(LL, ">$fn") || ($failed=$!);
        if ($failed) {
          ++$tries;
          print STDERR "[LOCK:$fn: $failed ...]\n";
          usleep($_LOG_LOCK_SLEEP);
        } else {
          flock(LL, LOCK_EX) || die "[LOCK:$fn: LOCK_EX: $!]\n";
          last;
        }
      }
      die "[LOCK:$fn: LOCK_EX failed after $tries tries]\n"
        if ($tries == $_LOG_LOCK_MAX_TRIES);
    }
  }
  sub errmsg {
    my $c = ($_[0] =~ /^\d+$/)? shift(@_): 1;
    my($pk,$fn,$line,$subr,$hasargs,$wantarray,$evaltext,$is_req) =caller($c);
    my $pkg= $pk;
    ($subr =~ /^(\S+)::(\S+)$/) && ($pkg = $1, $subr = $2);
    my $loc = "";
    $loc = "$fn:$line " if ($fn ne '-e');
    return logstamp(),"|ERR|[$loc$subr] @_";
  }
  sub debuglog {
    my($pk,$fn,$line,$subr,$hasargs,$wantarray,$evaltext,$is_req) = caller(1);
    my $pkg= $pk;
    ($subr =~ /^(\S+)::(\S+)$/) && ($pkg = $1, $subr = $2);
    my $dstr = "(\$" . $pkg . "::DEBUGGING == 1)";
    my $debugging = eval $dstr || 0;
    return unless $debugging;
    _loglock(1);
    print STDERR "[$fn:$line $subr] @_\n";
    _loglock(0);
  }
  sub testlog {
    my($pk,$fn,$line,$subr,$hasargs,$wantarray,$evaltext,$is_req) = caller(1);
    my $pkg= $pk;
    ($subr =~ /^(\S+)::(\S+)$/) && ($pkg = $1, $subr = $2);
    #print STDERR "testlog($pkg,$fn,$line,$subr): @_\n";
    my $tstr = "(\$" . $pkg . "::TESTING == 1)";
    my $testing = eval $tstr || 0;
    return unless $testing;
    my $loc = "";
    $loc = "$fn:$line " if ($fn ne '-e');
    _loglock(1);
    print STDERR logstamp(),"|TST|[$loc$subr] @_\n";
    _loglock(0);
  }
  sub modlog {
    my($pk,$fn,$line,$subr,$hasargs,$wantarray,$evaltext,$is_req) = caller(1);
    my $pkg= $pk;
    ($subr =~ /^(\S+)::(\S+)$/) && ($pkg = $1, $subr = $2);
    my $tstr = "(\$" . $pkg . "::TESTING == 1)";
    my $testing = eval $tstr || 0;
    my $logpstr = "\$" . $pkg . "::LOGPREF";
    my $logpref = eval $logpstr || "[?]";
    my $loglstr = "\$" . $pkg . "::LOGLEVEL";
    my $loglevel = eval $loglstr || 9;
    if ($testing) {
      my $loc = "";
      $loc = "$fn:$line " if ($fn ne '-e');
      _loglock(1);
      print STDERR logstamp(),"|MOD|[$loc$subr] @_\n";
      _loglock(0);
    }
    saylog($loglevel,$logpref."|MOD|@_");
  }
  sub errlog {
    my($pk,$fn,$line,$subr,$hasargs,$wantarray,$evaltext,$is_req) = caller(1);
    my $pkg= $pk;
    ($subr =~ /^(\S+)::(\S+)$/) && ($pkg = $1, $subr = $2);
    my $tstr = "(\$" . $pkg . "::TESTING == 1)";
    my $testing = eval $tstr || 0;
    my $logpstr = "\$" . $pkg . "::LOGPREF";
    my $logpref = eval $logpstr || "[?]";
    my $loglstr = "\$" . $pkg . "::LOGLEVEL";
    my $loglevel = eval $loglstr || 9;
    if ($testing) {
      my $loc = "";
      $loc = "$fn:$line " if ($fn ne '-e');
      _loglock(1);
      print STDERR logstamp(),"|ERR|[$loc$subr] @_\n";
      _loglock(0);
    }
    saylog(-1,$logpref."@_");
  }
  ## offsets in a mktime-style array
  use constant D_SEC   => 0;
  use constant D_MIN   => 1;
  use constant D_HOUR  => 2;
  use constant D_MDAY  => 3;
  use constant D_MON   => 4;
  use constant D_YEAR  => 5;
  use constant D_WDAY  => 6;
  use constant D_YDAY  => 7;
  use constant D_ISDST => 8;
}
##
# initprog - set $main::P (never export this, force a Util::initprog())
#
sub initprog {
  my @P = split('/', $main::0);
  $main::P = pop(@P);
}
##
# quitprog - shut down
#
sub quitprog {
  unlink($_LOG_LOCK) if (-f $_LOG_LOCK);
}
##
# dbdie - die because of a database error in a useful way
#
sub dbdie {
  my $dbe = $DBI::errstr;
  my $str = "@_ ($dbe)";
  saylog(-1, $str);
  die "$str ** FATAL ERROR **\n";
}
sub yyyyify {
  my $yy = shift(@_) + 0;
  return $yy if ($yy > 1000);
  return ($yy + 2000) if ($yy < 70);
  return 1900 + $yy;
}
sub mmify {
  my $mn = lc(shift(@_));
  return $MONTH_NAMES{$mn} || 0;
}
##
# parse_db_ts_Oracle - parse oracle timestamp
#
sub parse_db_ts_Oracle {
  my $ts = shift(@_);
  return 0 unless $ts =~ /^(\d\d)-(\S+)-(\d\d)$/;
  my($yyyy,$mm,$dd,$hh,$MM,$ss) = (yyyyify($3),mmify($2), 0+$1, 0, 0, 0);
  my $t = mktime($ss, $MM, $hh, $dd, $mm - 1, $yyyy - 1900);
  return $t;
}
##
# parse_db_ts_mysql - parse timestamp, mysql style
#
sub parse_db_ts_mysql {
  my $ts = shift(@_);
  return 0 unless $ts =~ /^(\d\d\d\d)(\d\d)(\d\d)(\d\d)(\d\d)(\d\d)$/;
  my($yyyy,$mm,$dd,$hh,$MM,$ss) = ($1, $2, $3, $4, $5, $6);
  my $t = mktime($ss, $MM, $hh, $dd, $mm - 1, $yyyy - 1900);
  return $t;
}
##
# parse_db_ts_Pg - parse timestamp, Postgres style
#
sub parse_db_ts_Pg {
  my $ts = shift(@_);
  my($yyyy,$mm,$dd,$hh,$MM,$ss);
  if ($ts =~ /^(\d\d\d\d)-(\d\d)-(\d\d)\s+(\d\d):(\d\d):(\d\d)/) {
    ($yyyy,$mm,$dd,$hh,$MM,$ss) = ($1, $2, $3, $4, $5, $6);
  } elsif ($ts =~ /^(\d\d\d\d)-(\d\d)-(\d\d)/) {
    ($yyyy,$mm,$dd,$hh,$MM,$ss) = ($1, $2, $3, 0, 0, 0);
  }
  my $t = mktime($ss, $MM, $hh, $dd, $mm - 1, $yyyy - 1900);
  return $t;
}
##
# parsets - parse timestamp
#
sub parse_db_ts {
  my $ts = shift(@_);
  my $dbh = shift(@_);
  my $if;
  $if = $dbh unless ref($dbh);
  unless ($if) {
    $if = $dbh->{Driver}->{Name} || "Pg"; # XXX gross
  }
  my $subr = "parse_db_ts_$if";
  my $sub = \&$subr;
  return &$subr($ts);
}
##
# gen_db_ts_Pg
#
sub gen_db_ts_Pg {
  my $t = shift(@_);
  return strftime("%Y-%m-%d %H%:%M:%S", localtime($t));
}
##
# gen_db_ts_Oracle
#
sub gen_db_ts_Oracle {
  my $t = shift(@_);
  return strftime("%Y-%m-%d %H%:M:%S", localtime($t));
}
##
# gen_db_ts_mysql
#
sub gen_db_ts_mysql {
  my $t = shift(@_);
  return strftime("%Y-%m-%d %H%:M:%S", localtime($t));
}
##
# gen_db_ts
#
sub gen_db_ts {
  my $t = shift(@_);
  my $dbh = shift(@_);
  my $if;
  $if = $dbh unless ref($dbh);
  unless ($if) {
    $if = $dbh->{Driver}->{Name} || "Pg"; # XXX gross
  }
  my $subr = "gen_db_ts_$if";
  my $sub = \&$subr;
  return &$subr($t);
}
##
# is_leapyear - given a year, return 1 if it is a leap year
#
sub is_leapyear {
  my $year = shift(@_);
  return 0 if (($year % 4) != 0);
  return 0 if (($year % 100) != 0);
  return 0 if (($year % 400) != 0);
  return 1;
}
##
# month_days - given a month (1..12) and a year (four digits) return #days
#
sub month_days {
  my($mon, $year) = @_;
  return $MONTH_DAYS{$mon} if ($mon != 2);
  return 28 if is_leapyear($year);
  return 29;
}
##
# get_monday - given a time_t, return the monday of that week
#
sub get_monday {                        # bob geldoff's favorite sub
  my $orig = shift(@_);
  my @d = (localtime($orig));
  my $delta;
  if ($d[D_WDAY] == 0) {
    $delta = -6;                        # sunday => previous monday
  } else {
    $delta = 0 - ($d[D_WDAY]-1);
  }
  my $mdays = month_days($d[D_MON]+1, $d[D_YEAR]+1900);
  my $isleap = is_leapyear($d[D_YEAR]+1900);
  my $niters = 0;
  while ($d[D_WDAY] != 1) {
    ++$niters;
    debuglog("iteration $niters: delta=$delta ".join(",",@d));
    if ($delta > 0) {
      $d[D_MDAY]++;
      $d[D_WDAY]++;
      if ($d[D_WDAY] == 7) {
        $d[D_WDAY] = 0;
      }
      $delta--;
    } else {
      $d[D_MDAY]--;
      $d[D_WDAY]--;
      if ($d[D_WDAY] < 0) {
        $d[D_WDAY] = 6;
      }
      $delta++;
    }
    if ($d[D_MDAY] <= 0) {
      $d[D_MON]--;
      if ($d[D_MON] < 0) {
        $d[D_MON] = 11;
        $d[D_YEAR]--;
      }
      $d[D_MDAY] = month_days($d[D_MON]+1, $d[D_YEAR]+1900);
    } elsif ($d[D_MDAY] > $mdays) {
      $d[D_MON]++;
      if ($d[D_MON] == 12) {
        $d[D_MON] = 0;
        $d[D_YEAR]++;
        $d[D_MDAY] = 1;
      }
    }
    $mdays = month_days($d[D_MON]+1, $d[D_YEAR]+1900);
    $isleap = is_leapyear($d[D_YEAR]+1900);
  }
  return mktime(@d);
}
##
# exclude - given a thing and an array, return a new array excluding the thing
#
sub exclude {
  my $thing = shift(@_);
  my @rez = ();
  foreach (@_) {
    push(@rez, $_) unless ($_ eq $thing);
  }
  return @rez;
}
##
# antivenom - remove nasty stuff from a string (like poison NUL)
#
sub antivenom {
  my $s = shift(@_);
  return "" unless $s;
  my $r = shift(@_) || "";
  #$s =~ s/^\./dot./;                    # no leading dots
  $s =~ s/[\x00-\x20\&\<\>\`\'\"\*\$\!\~\?]+/$r/g; # bad stuff => $r
  return $s;
}
##
# antivenom_lite - like antivenom, but with fewer calories
#
sub antivenom_lite {
  my $s = shift(@_);
  return "" unless $s;
  my $r = shift(@_) || "";
  ## my selection of control characters is based on my experience with
  ## stuff that causes problems: poison NUL, control-D, control-Z.
  ## ymmv.
  $s =~ s/[\x00\x04\x1A\&\<\>]+/$r/g; # bad stuff => $r
  return $s;
}
##
# antivenom_filename - get rid of badness in filenames/paths
#
sub antivenom_filename {
  my $name = shift(@_);
  return undef unless $name;
  $name = antivenom($name);
  my $dotsub = shift(@_) || "-";
  my $slashdotsub = shift(@_) || "_";
  $name =~ s/\/\./$slashdotsub/g;       # no dots after slashes
  $name =~ s/^\.+/$dotsub/g;            # no leading dots
  $name =~ s/^\/+//g;                   # no leading slashes
  $name =~ s/\/+$//g;                   # no trailing slashes
}
##
# logstamp - timestamp for a log entry in the format I prefer
#
sub logstamp {
  return strftime($LOGSTAMP_FMT, localtime time).($LOGSTAMP_SHORT?"":"|$$");
}
##
# saylog - log a message to somewhere, dealing with rotated logs etc.
#
sub saylog {
  #print "SAYLOG @_\n";
  my $level = shift(@_);
  my $L = $main::Verbose || $ENV{LOG_LEVEL};
  return if (($level >= 0) && ($L < $level));
  my $P = $main::P || "UNKNOWN";
  my $LOG_FILE = $main::LOG_FILE;
  my $LOG_FP;
  my $lfn = defined($LOG_FILE)? "$LOG_FILE..LCK": undef;
  _loglock(1, $lfn);
  if ($LOG_FILE) {
    $LOG_FP = $main::LOG_FP ||
      new IO::File($LOG_FILE, O_WRONLY|O_APPEND|O_CREAT) ||
        die "could not create logfile ($<,$[) $LOG_FILE: $!\n";
#    $LOG_FP->autoflush(1) if ref($LOG_FP) ne 'HASH';
  }
  my $LOG_STAMP = $main::LOG_STAMP;
  my $LOG_STDERR = $main::LOG_STDERR || defined($ENV{LOG_STDERR});
  $LOG_STDERR = 1 if !defined($LOG_FILE);
  my $LOG_FILE_SZ = $main::LOG_FILE_SZ;
  my $tstamp = logstamp();
  my $logstr = "$tstamp|$P|$level|@_\n";
  ## If we're logging to a file, it might get rotated, so check and deal
  if ($LOG_FILE) {
    my @junk;
    my $newsz;
    if (!(-f $LOG_FILE)) {
      $newsz = 0;
    } else {
      @junk = stat($LOG_FILE) || die "could not stat log file $LOG_FILE: $!\n";
      $newsz = $junk[7];
    }
    if ($newsz > $LOG_FILE_SZ) {
      $LOG_FILE_SZ = $newsz;
    } elsif ($newsz < $LOG_FILE_SZ) {
      ## Log file has been rotated -- reopen
      #print $LOG_FP "$tstamp|$P|$$|$level|abandoning rotated logfile\n"
      #  if $LOG_STAMP;
      print STDERR
        "$tstamp|$P|$$|$level|abandoning rotated logfile $LOG_FILE\n"
          if $LOG_STDERR;
      close($LOG_FP);
      my $tmp = $LOG_FILE . ".TMP";
      die "log file $LOG_FILE rotated and .TMP file already exists!"
        if -f $tmp;
      open(NLF, ">$tmp") ||
        die "opening new log file $tmp due to rotation: $!";
      ## fix
      my $oldfh = select(NLF);
      $| = 1;
      select($oldfh);
      $LOG_FP = \*NLF;
      rename($tmp, $LOG_FILE) ||
        die "renaming temporary log $tmp due to rotation";
      print $LOG_FP "$tstamp|$P|$$|$level|noticed logfile rotation (" .
        strftime("%Z", localtime time) . ")\n";
      print STDERR "$tstamp|$P|$$|$level|noticed logfile rotation\n"
        if $LOG_STDERR;
      @junk = lstat($LOG_FP) || die "second stat in saylog failed: $!";
      $LOG_FILE_SZ = $junk[7];
    }
  }
  $main::LOG_FP = $LOG_FP if $LOG_FP;
  print $LOG_FP $logstr   if $LOG_FP;
  print STDERR  $logstr   if $LOG_STDERR;
  _loglock(0, $lfn);
}
##
# plural - return plural form of n
#
sub plural {
  my($n,$u) = @_;
  return "$n $u" if $n == 1;
  return "$n $u" . "s";
}
##
# seconds -> human-readable statement of duration i.e. "1 hour, 3 minutes"
#
sub elapsed_string {
  my $deltat = shift(@_);
  return "0 sec" if !$deltat;
  my $dt = "";
  my $MINUTES = 60;
  my $HOURS = $MINUTES * 60;
  my $DAYS = $HOURS * 24;
  my $WEEKS = $DAYS * 7;
  my $MONTHS = $WEEKS * 4;
  if ($deltat > $MONTHS) {
    my $n = floor($deltat / $MONTHS);
    $deltat -= $n * $MONTHS;
    $dt .= " " if $dt ne '';
    $dt .= plural($n, "month");
  }
  if ($deltat > $WEEKS) {
    my $n = floor($deltat / $WEEKS);
    $deltat -= $n * $WEEKS;
    $dt .= " " if $dt ne '';
    $dt .= plural($n, "week");
  }
  if ($deltat > $DAYS) {
    my $n = floor($deltat / $DAYS);
    $deltat -= $n * $DAYS;
    $dt .= " " if $dt ne '';
    $dt .= plural($n, "day");
  }
  if ($deltat > $HOURS) {
    my $n = floor($deltat / $HOURS);
    $deltat -= $n * $HOURS;
    $dt .= " " if $dt ne '';
    $dt .= plural($n, "hr");
  }
  if ($deltat > $MINUTES) {
    my $n = floor($deltat / $MINUTES);
    $deltat -= $n * $MINUTES;
    $dt .= " " if $dt ne '';
    $dt .= plural($n, "min");
  }
  if ($deltat > 0) {
    $dt .= " " if $dt ne '';
    $dt .= plural($deltat, "sec");
  }
  return $dt;
}
##
# nl2br - perl version of php primitive of same name
#
sub nl2br {
  my $str = shift(@_);
  $str =~ s/\n/<br>\n/g;
  return $str;
}
##
# htmlspecialchars - perl version of php primitive of same name
#
sub htmlspecialchars {
  my $str = shift(@_);
  $str =~ s/\&/&amp;/g;
  $str =~ s/</&lt;/g;
  $str =~ s/>/&gt;/g;
  return $str;
}
##
# countchar - count the occurance of a specific character in a string
#
sub countchar {
  my $char = shift(@_);
  my $str = "@_";
  my $count = 0;
  my $pos;
  $pos = index($str, $char, $pos);
  while ($pos >= $[) {
    ++$count;
    ++$pos;
    $pos = index($str, $char, $pos);
  }
  return $count;
}
##
# is_printchar - is the given ascii value a printable character?
#
sub is_printchar {
  my $val = shift(@_);
  return (($val >= 35) && ($val <= 126));
}
##
# psychochomp - take off all leading and trailing spaces
sub psychochomp {
  my $str = "@_";
  $str =~ s/^\s+//;
  $str =~ s/\s+$//;
  return $str;
}
##
# indent - return a string of spaces
#
sub indent {
  my $n = shift(@_) || 0;
  return " " x $n;
}
##
# xml_walk - walk down an XML::TreeBuilder object and grab stuff from it
#
sub xml_walk {
  my $self = shift(@_);
  my $tree = $self;
  my $path = shift(@_);
  my @pelts = split(':', $path);
  my $elt = undef;
  my $helper;
  $helper = sub {
    my $elt = shift(@_);
    my $cname = shift(@_);
    return $elt unless $cname;
    my $i = 0;
    if ($cname =~ /^(\S+)\[(\d+)\]$/) {
      $i = $2;
      $cname = $1;
    }
    my $j = 0;
    my @kids = $elt->content_list();
    foreach my $kid (@kids) {
      if ($kid->tag eq $cname) {
        return &$helper($kid, @_) if ($j == $i);
        ++$j;
      }
    }
    return undef;
  };
  return &$helper($tree, @pelts);
}
##
# reach - reach into an XML::TreeBuilder object for something
#
# based on Command::AUTOLOAD
#
sub reach {
  my $self = shift(@_);
  my $name = shift(@_) || die "reach: no name\n";
  my $type = ref($self) || die errmsg("$self is not an object");
  my $tree = $self;
  $name =~ s/.*:://;
  my @rez = ();
  my $forcearray = 0;
  if (!scalar(@_)) {
    ## Case 0: a simple query (can't do walk with this because of syntax)
    @rez = ($tree->attr($name));
  } elsif ($_[0] =~ /\S+(:\S+|\[\d+\])/) {
    ## Case 1: query with a walk
    my $cname = shift(@_);
    $tree = xml_walk($tree, $cname);
    my $x = undef;
    if ($name eq '_') {
      $x = join("", map { !ref($_)? $_: "" } ($tree->content_list()));
    } else {
      $x = $tree->attr($name);
    }
    unshift(@rez, $x);
  } elsif ((scalar(@_) == 1) || (scalar(@_) == 2)) {
    ## Case 2: query with an index and possibly a walk
    my $cname = shift(@_);
    my $n = shift(@_) || 0;
    ## The order of these next two statements is important
    $forcearray = ($n < 0);
    $n = -1 if wantarray;
    my $i = 0;
    my @kids = $tree->content_list();
    foreach my $child (@kids) {
      if ($child->tag() eq $cname) {
        if ($i++ >= $n) {
          my $x = undef;
          if ($name eq '_') {
            $x = join("", map { !ref($_)? $_: "" } ($child->content_list()));
          } elsif ($name eq '__') {
            $x = $child;
          } else {
            $x = $child->attr($name);
          }
          unshift(@rez, $x);
          last unless ($n == -1);
        }
      }
    }
  }
  return (wantarray || $forcearray) ? @rez : $rez[0];
}
##
# reduce - what you'd think it is, but bletcherous and perly
#
sub _reduce1 {
  my $sub = shift(@_);
  my $partial = shift(@_);
  return $partial unless scalar(@_);
  $partial = &$sub($partial, shift(@_));
  return _reduce1($sub, $partial, @_);
}
sub reduce {
  ## XXX This is why perl is not LISP, even though it sometimes appears LISPy
  my $op = shift(@_);
  return eval join($op, @_) unless ref($op);
  die errmsg("reduce called with non-CODE ref") unless (ref($op) eq 'CODE');
  return _reduce1($op, @_);
}
##
# TEST - unit test
#
sub TEST {
  $TESTING = 1;
  $DEBUGGING = 9;
  testlog("TEST invoked with arguments: [@ARGV]");
  my $count = countchar('?', "select * from foo where a=? and b=? and c=?");
  testlog("count=$count");
}
##
## UNIVERSAL ###############################################################
##
## Methods to be inherited by all objects
##
package UNIVERSAL;
##
sub __filter_keys {
  my @rez = ();
  foreach (@_) {
    push @rez, $_ unless /^_/;
  }
  return @rez;
}
##
# to_string - make human-readable string
#
sub to_string {
  my $self = shift(@_);
  my $level = shift(@_) || 0;
  unshift(@_, $level) if (defined($level) && ($level !~ /^\d+$/));
  my %flags = @_;
  $level = $flags{level} if (!$level && defined($flags{level}));
  return "<<recursed too deep ($level)>>" if ($level > 10);
  my $indent = $flags{indent} || 2;
  my $terse = defined($flags{terse})? 1: 0;
  my $i = $terse? "": (" " x ($level * $indent));
  my $i2 = $terse? "": (" " x (($level+1) * $indent));
  my $r = $terse? " ": "\n";
  my $me = defined($self->{_PRINTNAME})? $self->{_PRINTNAME}: ref($self);
  my $rez = "$i<$me$r";
  my $helper;
  $helper = sub {
    my $_f = shift(@_);
    my $_x = defined($self->{$_f})? $self->{$_f}: undef;
    my $_r = ref($_x);
    my $_z;
    sub _quoteval {
      my $_v = shift(@_);
      return (($_v =~ /^\d+$/)? "$_v": "\"$_v\"");
    }
    if (!defined($_x)) {
      $_z = "";
    } elsif (!$_r) {
      $_z = "$_f="._quoteval($_x);
    } elsif ($_r eq 'ARRAY') {
      $_z = "$_f=[".join(",",map{ _quoteval($_) } @$_x)."]";
    } elsif ($_r eq 'HASH') {
      $_z = "$_f={".join(",",map{ "$_=>"._quoteval($_x->{$_}) } keys %$_x)."}";
    } elsif ($_r eq 'CODE') {
      $_z = "$_f=$_x";
    } elsif ($_x->can("to_string")) {
      $_z = "$_f=$r".$_x->to_string($level + 1, %flags);
    } else {
      $_z = "$_f=($_r)?$_x?";
    }
    return $_z;
  };
  if (defined($self->{_PRINTABLE})) {
    $rez .= join($r, map { $i2 . &$helper($_) } @{$self->{_PRINTABLE}});
  } elsif (ref($self) eq 'HASH') {
    $rez .= join($r, map { $i2 . &$helper($_) } keys %$self);
  } else {
    my @filtered_keys = __filter_keys(keys %$self);
    $rez .= join($r, map { $i2 . &$helper($_) } @filtered_keys);
  }
  $rez .= "$r$i>";
  return $rez;
}
##
1;
__END__

=head1 NAME

  Utils.pm - Sean's bag of tricks

=head1 SYNOPSIS

  use Utils;

  ## logging
  $main::LOG_FILE = '/path/to/logfile';
  $main::Verbose = 3;
  saylog($level,"foo");

  ## convenient death from DBI cause
  dbdie($errmsg);

  ## parse database time into time_t
  $time_t = parse_db_ts($row->{timestamp});

  ## absolute year, e.g. 1993
  is_leapyear($year);

  ## month 1..12, year absolute
  month_days($month,$year);

  ## return monday of week in which time_t falls
  ## week = monday..sunday.  returns time_t
  $monday_time_t = get_monday($time_t);

  ## givne thing and array, return array sans thing
  @array = exclude($bad_elt, @array);

  ## cleanse strings of various forms of bad stuff
  ## (poison NUL, "/.", control characters, etc).
  $str = antivenom($str);
  $str = antivenom_lite($str);
  $path = antivenom_filename($path);

  ## pluralize a noun
  print plural($n,"thing");

  ## given number of seconds, return human-readable string
  print "that took ",elapsed_string($nsecs);

  ## two php-compatibility functions
  print nl2br($some_html_string);
  print htmlspecialchars($some_html_string);

  ## count occurances of given character
  print "there are ",
        countchar('?',"select * from foo where a=?"),
        " paramters in the select statement\n";

=head1 DESCRIPTION

The Utils module contains a bunch of random subs and data that I find
useful in my day to day perl development.  These things don't really go
anywhere else.

By default, a subset of what is in Utils is exported.  Utils supports
the following symbolic tags when use'ing it:

  use Utils qw/:date/                        # date functions
  use Utils qw/:php/                         # php compatibility
  use Utils qw/:log/                         # logging funcitons
  use Utils qw/:web/                         # web programming utilities
  use Utils qw/:db/                          # database utilities
  use Utils qw/:string/                      # string utilities
  use Utils qw/:general/                     # general selection
  use Utils qw/:all/                         # everything

=head1 DESCRIPTION

The Utils module can export the following subs.  We list the export
tag in parenthesis after the name of the sub; "default" means that it is
exported by default (if no tag is given in the use statement).

=over

=item * saylog (default, :log)

Write an entry in the application log.  Hideously, but usefully, we
rely on three variables from the main:: namespace: $LOG_FILE, $LOG_FP
and $Verbose.  It so happens that my standard optionological setup
with Getopt::Long sets these by default, but ymmv.  $LOG_FILE is the
name of the file to drop logs into; it can be undef, in which case
$LOG_FP must be defined.  If $LOG_FP is defined, we write logs into
that file handle; if it is not defined, and $LOG_FILE is defined, then
we create (or truncate) the named file and set $LOG_FP to the handle
we open in doing so.

The first parameter to saylog is the level for the message.  If this
level is greater than $Verbose, we do nothing.  If the level is
negative, an attempt is always made to log the message (we assume
negative levels are serious errors).  We log the message using the
logstamp() sub to generate the timestamp; the format of the timestamp
is in the variable $Utils::LOGSTAMP_FMT, which can changed by client
code (but I don't recommend it).

The format of log entries is

  timestamp|progname|pid|msglevel|msg

Progname is the value of the optionally-set $main::P variable (which,
again, all my programs set, and which the Utils::initlog() sub will
set for you if you wish).  This is the last component of the $0 path.
Msglevel is the value of the first parameter to saylog.  Msg is
the concatenation of the rest of the arguments to saylog.  This format
is designed to be extensible and easy to parse; more fields can be
added beyond the required first four easily, without confusing a parser
that was designed to only parse the base message format.

Saylog is reasonably smart about log files being rotated out from
under it.  You must set $LOG_FILE for this logic to be triggered.  If
$LOG_FILE is defined, saylog checks to see that the size of the
logfile has not shrunk since the last time it was invoked.  If it has
shrunk, or if the file no longer exists, saylog assumes the logfile
has been rotated away, and starts a new one automatically.  This
behavior works will with both the BSD-style newsyslog(1) program and
various Linux-type utilities of the same ilk.

=item * dbdie (default, :db)

Useful with DBI-based programs; dbdie invokes saylog(-1,error-message)
and then die's.  The error message will include the value of $DBI::errstr.

=item * parse_db_ts (default, :db)

Parse_db_ts() parses a timestamp or datestamp value returned as a string
from some DBI driver module.  It currently knows about MySQL and Postgres
(and will be taught about Oracle Real Soon Now).  It returns a time_t
style result (the result of POSIX::mktime).

=item * is_leapyear (default, :date)

Given an absolute year (i.e. 1993), returns one if it is a leapyear, and
zero otherwise.

=item * month_days (:date)

Given a month (from 1 to 12) and an absolute year number, returns the number
of days in that month during that year.

=item * get_monday (:date)

Given a time_t style date/time, returns a time_t style date/time corresponding
to the same time on the monday of the week that the parameter falls in.
Weeks, for the purposes of get_monday(), start on Monday and end on Sunday,
so if you pass a time_t that falls on a Sunday, the previous Monday will
be returned.

=item * exclude (:general)

Given a thing and an list of things, returns a new array that is the
same as the given list of things, sans the initial thing.  I don't know
why perl doesn't have something like this built into it, it has every
other bloody thing.

=item * antivenom (default, :web)

The strongest form of the antivenom* set of subs, this variant takes a
string and removes all NUL bytes, control characters, ampersands,
quotation marks of all kinds, angle brackets, stars, dollar signs,
exclaimation points, tildes, and question marks.  In other words, all
of the things in, e.g. web form inputs, that typically can cause
applications to do bad things.

=item * antivenom_lite (default, :web)

This is a variant of antivenom() that only removes NUL, control-D,
control-Z, ampersand and angle brackets from its argument, which are
all characters that I have found can cause consternation to web
applications that are insufficiently careful with their inputs.

=item * antivenom_filename (default, :web)

Invokes antivenom on its argument and then substitutes a single
dash for all of the following:

   all occurances of slash followed by dot
   any sequence of one or more dots still left
   any sequence of one or more slashes at the start of the string
   any sequence of one or more slashes at the end of the string

If you want something other than a single dash (-) substituted for
these things, you can pass it as the second argument.  These are the
things that crafty crackers typically try to use to fool web
applications into doing bad things to the filespace on their servers.

=item * plural (:general, :string)

Given a noun and a number, return that noun pluralized for the given
number.

=item * elapsed_string (:general, :string)

Given a number of seconds, return a string like

  1 day, 14 hours, 43 minutes and 15 seconds

=item * nl2br (default, :php)

A perl equivalent for the function of the same name in PHP.

=item * htmlspecialchars (default, :php)

A perl equivalent for the function of the same name in PHP.

=item * countchar (default, :string, :general)

Given a character and a string, return the number of times the
character occurs in the string.

=back

=head1 AUTHOR

  Sean Levy <snl@cert.org>

=head1 SEE ALSO

=over

=item * perl(1).

=back

=cut

# Local variables:
# tab-width: 2
# perl-indent-level: 2
# indent-tabs-mode: nil
# comment-column: 40
# time-stamp-line-limit: 16
# End:
