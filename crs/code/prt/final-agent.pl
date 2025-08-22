#!/usr/bin/env perl
# final-agent.pl
# this is a generified version of ensure-patch-cleanup.pl which is intended
# to start as the first, daemonized agent of a PRT test... and will invoke
# the command (and optional args) on this command line upon completion of the test.
# This idiom is useful to run a final agent regardless of the success of
# preceding agents.

use strict;
use warnings;

use File::Copy;
use FindBin;
use Getopt::Long;
use Pod::Usage qw ( pod2usage );
use File::Basename qw( fileparse );
use POSIX;
use Time::HiRes qw( gettimeofday );
use IO::Handle; # for autoflush

my ($program, $pdir, $psuffix) = fileparse($0, qr/\.[^.]*/);
$program = "$program$psuffix";
my $VERBOSE = 1;

# note trailing blank, but no newline
sub timestamp() {
    my ($utc, $microseconds) = gettimeofday;
    my $stamp = POSIX::strftime("[%m/%d/%Y %H:%M:%S", localtime($utc));
    $stamp .= sprintf(".%06d] ", $microseconds);
    return $stamp;
}

# [tmarble:20121128.1052CST] do not buffer output
STDOUT->autoflush(1);
STDERR->autoflush(1);

# [tmarble:20111027.1115CST] Discussed this approach with engstrom:
# if there is only one arg then we are NOT using printf so just
# use print (NOTE: this avoids unintentional printf char interpretation)
sub verbose(@) {
  if ($VERBOSE) {
      print STDERR timestamp();
      if (scalar(@_) == 1) {
          print STDERR @_;
      } else {
          printf(STDERR @_);
      }
  }
}

sub read_filestring {
    my $fn = shift;
    my $str = "";
    if (-e $fn) {
	my $fh;
	open($fh, "<$fn");
	while (<$fh>) {
	    # chomp;
	    $str .= $_;
	}
	close($fh);
    }
    return $str;
}

sub read_pidfile {
  my $fn = shift;
  my $str = read_filestring($fn);
  $str =~ s/\n$//; # remove newline
  if (length($str) > 0) {
      $str = 0 + $str; # convert to int
  } else {
      $str = -2;
  }
  return $str;
}

my $cmd;
my $pidfile;

# --pidfile PIDFILE
# WILL kill the process specified by the pid in PIDFILE
# else
# we'll add all the args as one command and execute it

if ( scalar(@ARGV) == 2 &&
     $ARGV[0] eq "--pidfile" ) {
    $pidfile = $ARGV[1];
    verbose("$program: upon completion will signal the process in: $pidfile\n");
} else {
    $cmd = join(" ", @ARGV);
    verbose("$program: upon completion will exec: $cmd\n");
}

# Set our signal handlers so that we will die, this should cause the
# END block to be called.
$SIG{TERM} = $SIG{INT} = $SIG{QUIT} = $SIG{HUP} = sub { exit; };

verbose("$program: going to sleep.\n");
sleep();

# Define an END block which will be called when PRT exits.
END {
    if (defined($cmd)) {
        verbose("$program: exec: $cmd\n");
        # [tmarble:20130307.0942CST] pause 2 seconds here because some uses of this script
        # involve processing output from other PRT agents... which might not have finished
        # flushing to disk yet...
        sleep(2);
        system($cmd);
    } elsif (defined($pidfile)) {
        my $pid = read_pidfile($pidfile);
        if ($pid > 0) {
            if (kill(0, $pid)) {
                # verbose("$program: $pid is running\n");
                verbose("$program: killing pid $pid\n");
                $! = 0;
                kill(15, $pid); # SIGTERM
                my $result = "$? => " . $!;
                verbose("$program: kill result: $result\n");
            } else {
                verbose("$program: $pid is NOT running\n");
            }
        }
    }
}


# ------------------------------------------------------------
## POD USAGE
__END__

=head1 NAME

final-agent - Used in PRT as the final agent

=head1 SYNOPSIS

final-agent CMD [ARGS]

=head1 DESCRIPTION

B<final-agent.pl>  sleep and then on exit run CMD ARGS...

=head1 OPTIONS

=head2 Generic Options:

   NONE

=head1 AUTHOR(S)
