#!/usr/bin/env perl

# Note we mostly use stdout vs, eg, die() and stderr b/c when this gets called from lisp we often only get stdout to logs
# and it is easier to handle that here than remember to always get stderr in the calls to this.

# Here's a couple examples of how to use the new trigger and action args:

#musliner@cgc-submit:~/neo-fuzz/code/prt$ echo "voltage 9" >test
#musliner@cgc-submit:~/neo-fuzz/code/prt$ ./logwatcher.pl -t "voltage (\d+)" -f test -a "print \"\$1 foo\n\";"
#Looking for patterns and antipatterns in test
#Found trigger [voltage (\d+)] in line 1:
#voltage 9
#action is print "$1 foo\n";
#9 foo
#^C
#musliner@cgc-submit:~/neo-fuzz/code/prt$ echo "dir foo" > test
#musliner@cgc-submit:~/neo-fuzz/code/prt$ mkdir foo
#musliner@cgc-submit:~/neo-fuzz/code/prt$ touch foo/boo
#musliner@cgc-submit:~/neo-fuzz/code/prt$ ./logwatcher.pl -t "dir (\w+)" -f test -a "system(\"ls \$1\");"
#Looking for patterns and antipatterns in test
#Found trigger [dir (\w+)] in line 1:
#dir foo
#action is system("ls $1");
#boo
#^C


use strict;
use warnings;

use Getopt::Long;

# Turn on autoflushing.
$| = 1;

my $echo_inputs = 0;
my $filename;
my $dockername;
my $sleeptime;
my @patterns = ();
my @antipatterns = ();
my @triggers = ();
my @actions = ();

GetOptions('e|echo_inputs'        => \$echo_inputs,
	   'f|filename=s' => \$filename,
	   'd|dockername=s' => \$dockername,
	   's|sleeptime=s' => \$sleeptime,
	   'p|pattern=s'          => \@patterns,
	   'q|antipattern=s'      => \@antipatterns,
	   't|trigger=s'      => \@triggers,
	   'a|action=s'      => \@actions,
	  )
  or die "Error in options parsing.";

if (!defined($filename) && !defined($dockername)) {
    print("Neither filename nor dockername specified; how would I know what to watch?  Fail.\n"); exit(1);
}

# loop and sleep until the file is created
if (defined($filename)) 
  { 
	while (not -e $filename) { sleep .1; } 
 	print "Looking for patterns and antipatterns in $filename\n";
  }

my $fb_handle;
my $pid;

#print "Patterns: @patterns\n";
#print "AntiPatterns: @antipatterns\n";
  foreach my $pattern (@patterns) { print "Pattern: [$pattern]\n"; }
  foreach my $pattern (@antipatterns) { print "AntiPattern: [$pattern]\n"; }

if ($filename && ! -f $filename) {
    print("filename does not exist: $filename\n");
    exit(1);
}

# if you call this in docker mode and the docker was just created, it might 
# still be in created status and not have an actual docker logfile to open.  Must wait a sec.
my $create_count=0;
if (defined($dockername)) {
  my $cmd= "docker ps -q --filter status=created --filter \"name=$dockername\"";
  while (`$cmd` && $create_count++<40) {
    print("Docker is in created state; waiting");
    sleep(.1);
}}

if ($create_count >=20) { print "\nDocker container $dockername appears stuck in 'created' mode, see docker ps -a\n" ; exit(1);}

if ($filename) {
  $pid = open($fb_handle, "-|", "tail -n +0 -f $filename");
  } elsif ($dockername) {
  $pid = open($fb_handle, "-|", "docker logs -f $dockername");
  } else { print "No filename or dockername provided\n"; exit(1); }

if (!$pid) { print "Error opening logfile or docker logs\n";  exit(1);}

my $found = -1;
my $pattern;
my $action;
my $trigger;
while($found < 0) {
  my $line = <$fb_handle>;
  if (!defined($line)) { print "Error getting a line from logfile or docker logs\n"; exit(1); }
  $echo_inputs and
    print("$line");
  foreach $pattern (@patterns) {
    if($line =~ /$pattern/) {
      print("Found pattern [$pattern] in line $.:\n$line");
      $found = 0;
    }
  }
  foreach $pattern (@antipatterns) {
    if($line =~ /$pattern/) {
      print("Found antipattern [$pattern] in line $.:\n$line");
      $found = 1;
    }
  }
  foreach $trigger (@triggers) {
    if($line =~ /$trigger/) {
      print("Found trigger [$trigger] in line $.:\n$line");
      foreach $action (@actions) {
        print "action is $action\n";
        eval($action);
      }
    }
  }
}

if ($sleeptime) {sleep($sleeptime);}

#print("Killing tail (pid: $pid).\n");
kill('TERM', $pid);
#print("Closing file handle.\n");
close($fb_handle);
print("Exiting with status $found.\n");
exit($found);
