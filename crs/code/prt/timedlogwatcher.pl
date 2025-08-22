#!/usr/bin/env perl

use strict;
use warnings;

use Getopt::Long;

# Turn on autoflushing.
$| = 1;

my $echo_inputs = 0;
my $filename;
my @patterns = ();
my @antipatterns = ();
my $timeout;

GetOptions('e|echo_inputs'        => \$echo_inputs,
	   'f|filename=s' => \$filename,
	   'p|pattern=s'          => \@patterns,
	   'q|antipattern=s'      => \@antipatterns,
	   't|timeout=s'		  => \$timeout,
	  )
  or die "Error in options parsing.";

if (!defined($filename)) {
    print("filename not specified.\n");
    exit(1);
}

my $fb_handle;

use POSIX qw(strftime);
my $stop_time = time + $timeout;

print "Looking for patterns and antipatterns in $filename with timeout $timeout\n";
#print "Patterns: @patterns\n";
#print "AntiPatterns: @antipatterns\n";
  foreach my $pattern (@patterns) { print "Pattern: [$pattern]\n"; }
  foreach my $pattern (@antipatterns) { print "AntiPattern: [$pattern]\n"; }

# loop and sleep until the file is created, or timeout
while (not -e $filename and time < $stop_time) { sleep 1; }

if (! -f $filename) {
    print("filename does not exist: $filename\n");
    exit(1);
}

open($fb_handle, "<", $filename)
  or die "Cannot open file: $filename";

my $found = -1;
my $pattern;
while($found < 0 and time < $stop_time) {
  my $line = <$fb_handle>;
  if (defined $line) {
	  $echo_inputs and print("$line");
	  
	  foreach $pattern (@patterns) {
		if($line =~ /$pattern/i) {
		  print("Found pattern [$pattern] in line:\n$line\n");
		  $found = 0;
		}
	  }
	  foreach $pattern (@antipatterns) {
		if($line =~ /$pattern/i) {
		  print("Found antipattern [$pattern] in line:\n$line\n");
		  $found = 1;
		}
	  }
  }
  else {sleep(1);}
}

if($found < 0) {
  # time expired without finding a pattern or error
  if(@patterns > 0) {
    $found = 1;
  }
  else {
    $found = 0;
  }
}

close($fb_handle);
exit($found);
