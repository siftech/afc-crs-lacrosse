#!/usr/bin/perl -w
# LACROSSE startup
# this is a hugely torn-down version of the old CGC-era 'doit' monstrosity.  Instead of using libraries to hold its functions
# and a zillion OS-level things replicated, it can only start NF across a cluster, by writing a doit.sh script that does that.
# And pretty much nothing else.  Rejoice!
# Unlike regular neofuzz-launcher.pl, this one supports running the SBCL docker agents vs host-machine ACL agents.

# args: jobdir lispcode numoptimi numfbs localhostonly usegui allocationmethod

# if localhostonly is set to anything, numoptimi and numfbs are ignored and only one Optimus is run on localhost.

use warnings;
use strict;
use File::Copy;
use File::Basename qw ( fileparse );
use POSIX;
use Sys::Hostname;
use Cwd qw( abs_path );
use List::Util qw(reduce);
use Data::Dumper qw(Dumper);
use Getopt::Long;
use Pod::Usage qw( pod2usage );
use File::Find;

our $fuzzbombs_per_slot = 1;

# program args  --------------------------------------------------
# FIXME Use a standard getopt package.

warn "neofuzz-sbcl-launcher.pl args: @ARGV\n";
our $jobdir=shift();
our $lisp = shift();
our $optimi = shift();
our $fbs = shift();
our $localhostonly = shift();
our $usegui = shift();
our $allocationmethod = shift();
our $justdontdoit = shift();

if ((not (defined $lisp)) || $lisp eq "") {
  die "must specify lispcode\n";
}
if (!$localhostonly && (!$optimi || $optimi < 1)) {
  die "must specify positive number of Optimi\n";
}
if (!$localhostonly && ($fbs < 0)) {
  die "must specify non-negative number of Fuzzbombs\n";
}
if ($localhostonly) { warn("launching one neofuzz on local host only; numoptimi and numfbs ignored...\n"); $fbs=0;$optimi=1;}
else {warn("launching neofuzz with $optimi optimi, $fbs fuzzbombs...\n");}

if ((not (defined $allocationmethod)) || $allocationmethod eq "") {
  $allocationmethod = 'default';
}
if (not (($allocationmethod eq 'default')
         || ($allocationmethod eq 'balanced')
         || ($allocationmethod eq 'justthishost')))
{ die "Unsupported allocationmethod: $allocationmethod"; }

#----------------------------------------------
# file locking

use Fcntl qw(:flock SEEK_END);

sub lock {
  my ($fh) = @_;
  flock($fh, LOCK_EX) or die "Cannot lock file $!\n";
  # and, in case someone appended while we were waiting...
  #seek($fh, 0, SEEK_END) or die "Cannot seek - $!\n";
}

sub unlock {
  my ($fh) = @_;
  flock($fh, LOCK_UN) or die "Cannot unlock file- $!\n";
}

#----------------------------------------------
# here's stuff to use a persistent /tmp file to hold next cluster identifier and other host load data
# see https://www.oreilly.com/library/view/mastering-perl/9780596527242/ch14.html

## NOTE when the tasks end, we must of course ensure that /tmp/cluster-allocation is revised to release the resources.
## this is done by release-cluster and release-my-clusters tool.

# uid for cluster being created.  Will be initialized to $nextcluster + 1 when cluster-allocation file is read.
our $cluster = 1;
our @chosen_hosts;
our (@hosts, %cores, %used, %cluster_hosts, %cluster_owners, $nextcluster);
our $totalcores=0;
our $totalused=0;
our $fh_cluster_allocation;

sub read_cluster_allocation {
  my $fn = "/tmp/cluster-allocation";
  if (! -e $fn) {
    warn "$fn doesnt exist: $!; resetting";
    system("$main::cgc/rsync/code/tools/reset-cluster-allocations");
  }
  my $data = do {
    if (open $fh_cluster_allocation, '+<', $fn) { lock($fh_cluster_allocation); local $/; <$fh_cluster_allocation> }
    else { undef }
  };
  warn "about to eval old data to get status\n";
  warn "data is:\n$data\n";
  eval $data; 	# this reloads the hashes from $fn, supporting the strict predeclared vars
  warn "finished eval of old data\n";
  warn "Hosts are:\n";
  warn "$_\n" for @hosts;

  $cluster = $nextcluster;
  $nextcluster++;

  for my $host (@hosts) {
    $totalcores += $cores{$host};
    $totalused += $used{$host};
  }

  warn "Total cores: $totalcores, total used: $totalused\n";
}

sub choose_hosts {
  my ($needed) = @_;
  my @result;

  warn "asked to choose $needed hosts\n";

  # allocate if possible
  if ($needed > ($totalcores - $totalused)) { die "ERROR: not enough host cores available for all the specified optimi and fuzzbombs"; }

  $cluster_owners{$cluster}=$ENV{'USER'};
  while ($needed>0) {
    # sort the hosts by availability level so pick unloaded ones first
    my @sorted_hosts= sort { ($cores{$a}-$used{$a}) <=> ($cores{$b}-$used{$b}) } @hosts;
    my $curhost = pop @sorted_hosts;
    my $avail = $cores{$curhost}-$used{$curhost};
    my $give=0;
    if ($avail>0 && $needed > $avail) { $give = $avail; } else {$give=$needed;}
    $used{$curhost}+=$give;
    $needed -= $give;
    for my $i (1..$give)
    { push @result, $curhost;
      $cluster_hosts{$cluster}= join(' ', @result);
    }
  }
  return @result;
}

sub choose_balanced_hosts {
  my ($needed) = @_;
  my @result;
  warn "asked to choose $needed hosts (balanced).\n";
  my $num_hosts = scalar(@hosts);
  warn "num_hosts: $num_hosts\n";
  my @spread = spread($needed, $num_hosts);
  warn "spread: @spread\n";
  # feasible?
  for (my $i=0; $i<$num_hosts; $i++) {
    my $avail = $cores{$hosts[$i]} - $used{$hosts[$i]};
    warn "avail: $avail  required: $spread[$i]\n";
    if ($avail < $spread[$i]) {die "ERROR: not enough host cores available for requested balanced spread";};
  }
  # allocate
  $cluster_owners{$cluster}=$ENV{'USER'};
  for (my $i=0; $i<$num_hosts; $i++) {
    my $curhost = $hosts[$i];
    my $give = $spread[$i];
    $used{$curhost}+=$give;
    for my $j (1..$give) {
      push @result, $curhost;
      $cluster_hosts{$cluster}= join(' ', @result);
    }
  }
  return @result;
}

sub choose_justthishost {
  my ($needed) = @_;
  my @result;
  warn "asked to start $needed agents on justthishost.\n";
  my $avail = $cores{$main::hostname}-$used{$main::hostname};
  if ($needed > $avail) { die "ERROR: not enough host cores available for all the specified optimi and fuzzbombs"; }
  $cluster_owners{$cluster}=$ENV{'USER'};
  my $give=$needed;
  $used{$main::hostname}+=$give;
  for my $i (1..$give)
  { push @result, $main::hostname;
    $cluster_hosts{$cluster}= join(' ', @result);
  }
  return @result;
}


# return list of $bins numbers summing to $total.
sub spread {
  my ($total, $bins) = @_;
  my $floor = int($total / $bins);
  my $rem = $total % $bins;
  # init array values to $floor (parens are necessary for perl's WWLD type system)
  my @retval = ($floor) x $bins;
  # spread remainder
  for (my $i=0; $i < $rem; $i++) {
    $retval[$i] = $retval[$i] + 1;
  }
  return @retval;
}

sub write_cluster_allocation {
  # write the revised status back out
  seek($fh_cluster_allocation, 0, 0) or die "Cannot seek - $!\n"; # go back to start of file
  truncate $fh_cluster_allocation,0;

  my $dd = Data::Dumper->new([\@hosts, \%cores, \%used, \%cluster_hosts, \%cluster_owners, $nextcluster],[qw(*hosts *cores *used *cluster_hosts *cluster_owners *nextcluster)]);
  print $fh_cluster_allocation $dd->Dump;
  unlock($fh_cluster_allocation);
  close $fh_cluster_allocation;
}

sub make_amp {
  my ($name, $role) = @_;
  $name = uc($name);
  my $str = "(if (or (string= \"$name\" \"OPTIMUS0\")  (string= common-lisp-user::*fb-instance* \"OPTIMUS0\") (string= common-lisp-user::*fb-instance* \"$name\")) (make-amp \"$name\"";
  $str .= " :master-p T" if ($name eq "OPTIMUS0");
  $str .= " :role $role))\n";
  return $str;
}

sub write_filestring {
  my $fn = shift;
  my $str = shift;
  my $fh;
  open($fh, ">$fn");
  $fh->autoflush(1);
  print $fh $str;
  close($fh);
}

# the old start of doit

our ($program, $pdir, $psuffix) = fileparse($0, qr/\.[^.]*/);
$program = "nf";
# current working directory
our $dir = abs_path(".");
our $basename = "nf";
if (defined($ENV{CIRCA_BASENAME})) {
  $basename = $ENV{CIRCA_BASENAME};
} elsif (defined($ENV{USER})) {
  $basename = $ENV{USER};
}
our $baseport = 10000;
if (defined($ENV{CIRCA_BASEPORT})) {
  $baseport = $ENV{CIRCA_BASEPORT};
}

our $matchmakeroffset = 2;
#our $usertmp = "/tmp/$basename";
#if (! (-d $usertmp)) { mkdir($usertmp) };
#die "Couldn't make directory $usertmp" unless -d "$usertmp";
our $hostname = `hostname -f`;
chomp $hostname;
our $docker_tag = "latest";
if (defined($ENV{DOCKER_TAG})) {
  $docker_tag = $ENV{DOCKER_TAG};
}

#Needed for rootless Docker
our $docker_host = "";
if (defined($ENV{DOCKER_HOST})) {
  $docker_host = $ENV{DOCKER_HOST};
}
our $loopback = "";
if (defined($ENV{LOOPBACK})) {
  $loopback = $ENV{LOOPBACK};
}

STDOUT->autoflush(1);
STDERR->autoflush(1);
our $VERBOSE = 0;
our $TIMESTAMP = 1;

our $home = $ENV{HOME};
our $cgc = abs_path("$main::pdir../..");

our $log; # file handle
our $experiment = "";
our $submit = "";
our $shareddir = "$cgc/scratch";
our $shared = $ENV{DOIT_SHARED} || $shareddir;
our $owner = $ENV{DOIT_OWNER} || $basename;

# data structures shared between modules
our %fuzzbombs = ();

warn("verbose: $VERBOSE\n");
warn("program: $program\n");
warn("dir: $dir\n");
warn("hostname: $hostname\n");
warn("CIRCA_BASEPORT: $baseport\n");
warn("CIRCA_BASENAME: $basename\n");
warn("home: $home\n");
warn("cgc: $cgc\n");
warn("main::hostname: $main::hostname\n");
warn("main::cgc: $main::cgc\n");

# Generate experiment.lisp from template

$lisp = "(cl:in-package :fuzzbomb)\n" . $lisp;	# preface w/ in-package

#$lisp .= "(pushnew :all *debug-list*)\n"; # rm this when things are rocking.
$lisp .= "(dbug :top \"This instance is: ~A\" common-lisp-user::*fb-instance*)\n";
$lisp .= "(setf *connect-to-bridge* t)\n";
$lisp .= "(setf *run-acceptor* t)\n";
$lisp .= ";; required make-amp calls\n";
my $ocount = 0;
my $fcount = 0;
my @tasks = ();
my (@os, @fs);
while ($ocount < $optimi) {
  my $name = "OPTIMUS$ocount";
  push(@os, $name);
  $lisp .= make_amp($name, ":optimus");
  ++$ocount;
}
while ($fcount < $fbs) {
  my $name = "FUZZBOMB$fcount";
  push(@fs, $name);
  $lisp .= make_amp($name, ":fuzzbomb");
  ++$fcount;
}
my $oprime = $os[0];

if ($usegui) {
  my $gui = "$oprime-GUI";
  $lisp .= "(dbug :top \"GUI name is $gui\")\n";
  $lisp .= make_amp($gui, ":gui");
  $lisp .= "(setf *use-gui* t)";
}

##$lisp .= "(setf *optimus-prime* \"$oprime\")\n";
$lisp .= "(setf *optimi* '(\"" . join('" "', @os) . "\"))\n";
$lisp .= "(setf *fuzzbombs* '(\"" . join('" "', @fs) . "\"))\n";
$lisp .= "(setf *dvms* nil)\n";

$lisp .= "(dbug :top \"Finished loading experiment.lisp\")\n";
# this was in case needed to push run-amp call into here.
#$lisp .= "(dbug :top \"Finished loading experiment.lisp, about to call run-amp\")\n";
#$lisp .= "(fb::run-amp :name common-lisp-user::*fb-instance*)\n";

write_filestring("$main::cgc/code/experiment.lisp", $lisp);

# we actually do rsync even if localhostonly, so the /rsync code area can be used in all cases.
if (!$localhostonly) {
# rsync moved to prt's common.pl but FIXME will need to create experiment.lisp and rsync later or create before rsync
# so for now, this just repeats the rsync, which should be very fast b/c only experiment.lisp changed
warn("Start copying experiment.lisp to all cluster nodes...\n");
## FIXME should just rsync the experiment.lisp file if we are already ensuring rsync-code before doit call
system("$main::cgc/code/tools/rsync-code");
system("$main::cgc/code/tools/rsync-experiment $localhostonly");
warn("Finished copying code to all cluster nodes.\n");
}

# note the driver prt file must have ensured matchmaker is running

# We need to know how many CPU cores are on each host... could can that or re-query each time.
# Easier for now to can it.
# And eventually we'll find the host names by querying Spark or Ambari (or condor) or somesuch
# So FIXME for now we can use our old friend list-condor-nodes.

#- idea for trivial node locking: have a file with a perl-formatted hash or two you lock (or wait), load, check for capacity and modify if avail,
#then write back out and unlock.  No parsing required and easily readable by human too.  Just needs hostname, totalcapacity, inuse.

if ($localhostonly) {
  @chosen_hosts= ( $hostname );
} else {
  read_cluster_allocation();
  if ($allocationmethod eq 'default') {
    @chosen_hosts= choose_hosts(int($optimi)+int($fbs));
  } elsif ($allocationmethod eq 'balanced') {
    @chosen_hosts = choose_balanced_hosts(int($optimi)+int($fbs));
  } elsif ($allocationmethod eq 'justthishost') {
    @chosen_hosts = choose_justthishost(int($optimi)+int($fbs));
  } else {
    die "Unsupported allocationmethod: $allocationmethod";
  }
  warn "chosen hosts are @chosen_hosts";
  write_cluster_allocation();
}

if ($#chosen_hosts < $#os + $#fs) { die "ERROR: not enough hosts chosen for all the specified optimi and fuzzbombs"; }

warn("creating ssh launcher file\n");
my $sub = "";
my $ampcount = 0;
my %used_hosts;
my $mm_host;

# Determine ports needed for OPTIMUS
my $optimi_port = $baseport;

# give each optimus it's own slot
while (@os) {
  my $name = shift(@os);
  my $newhost = shift(@chosen_hosts);
  $used_hosts{$newhost}=1;
  warn "comparing $newhost and $main::hostname and $ENV{LOOPBACK}";
  # Lacrosse fbs are referencing hostname
  # if ($newhost eq $main::hostname && defined($ENV{LOOPBACK})) {
  #   $mm_host = $main::loopback;
  # } else {
  #   $mm_host = $main::hostname;
  # }
  #
  $mm_host = $main::hostname;

#  my $args = "$mm_host $main::baseport $name";
  my $args = "sbcl";

  warn("args are: $args");

  my $optimi_circa_port = $optimi_port + 3;
  my $optimi_bridge_port = $optimi_port + 4;
  $optimi_port+=2;
  my $log_path="$jobdir/$name.log";
  my $inner_log_path="/logdir/$name.log";
  my $send_telem_path="$jobdir/send-telemetry-$name.log";

  # Note we prob dont have to set DOCKER_HOST anymore since tools/docker does it every time
  $sub .= join(" ",
               "$cgc/rsync/code/tools/sshc",
               $newhost,
               #"CIRCA_MM_HOST=$mm_host",
               #"DOCKER_HOST=$docker_host",
               #"CIRCA_PORT=$optimi_circa_port",
               #"BRIDGE_PORT=$optimi_bridge_port",
               #"'$cgc/rsync/code/tools/docker exec $basename-neo-fuzz-ccl /realuser.sh bash --login -c \\\"$ENV{LACROSSE_HOME}/rsync/code/tools/run-ccl-agent $args\\\"'",
               "'$cgc/rsync/code/tools/docker exec $basename-neo-fuzz-ccl /realuser.sh bash --login -c \\\"CIRCA_MM_HOST=$mm_host CIRCA_HOST=$newhost CIRCA_PORT=$optimi_circa_port BRIDGE_PORT=$optimi_bridge_port \\\\\\\${DOCKER_LACROSSE_HOME}/code/tools/lax-run-optimus0 $args\\\"'",
               "> $log_path 2>&1 &\n");

  $sub .= join(" ",
               "$cgc/rsync/code/tools/sshc $mm_host '$cgc/rsync/code/tools/docker exec $basename-neo-fuzz-ccl /realuser.sh bash --login -c \\\"CIRCA_MM_HOST=$mm_host CIRCA_HOST=$newhost CIRCA_PORT=$optimi_circa_port BRIDGE_PORT=$optimi_bridge_port \\\\\\\${DOCKER_LACROSSE_HOME}/code/tools/send-telemetry-logfile.py OPTIMUS0 $inner_log_path\\\"'",
               "> $send_telem_path 2>&1 &\n");
  ++$ampcount;
}

# Determine ports needed for OPTIMUS
my $fuzzbomb_port = $baseport + 100;

while (@fs) {
  my $fbs_in_slot = 0;
  while (@fs && $fbs_in_slot < $main::fuzzbombs_per_slot) {
    my $name = shift(@fs);
    my $newhost = shift(@chosen_hosts);
    $used_hosts{$newhost}=1;
    # if ($newhost eq $hostname && defined($ENV{LOOPBACK})) {
    #   $mm_host = $main::loopback;
    # } else {
    #   $mm_host = $main::hostname;
    # }
    #$mm_host = $main::hostname;
  #	are using shortname in $main::hostname, at this point
  $mm_host = `hostname -f`;
  chomp $mm_host;

    my $args = "$mm_host $baseport $name";
    #$sub .= "$cgc/code/tools/sshc $newhost CIRCA_MM_HOST=$mm_host DOCKER_HOST=$docker_host '$cgc/rsync/code/tools/docker exec $basename-neo-fuzz-ccl /realuser.sh bash --login -c \\\"$ENV{LACROSSE_HOME}/rsync/code/tools/run-ccl-agent $args\\\"' > $name.log 2>&1 &\n";
    my $fuzzbomb_circa_port = $fuzzbomb_port;
    my $log_path="$jobdir/$name.log";
    my $inner_log_path="/logdir/$name.log";
    my $send_telem_path="$jobdir/send-telemetry-$name.log";
    $fuzzbomb_port+=1;
    $sub .= "$cgc/rsync/code/tools/sshc $newhost '$cgc/rsync/code/tools/docker exec $basename-neo-fuzz-ccl /realuser.sh bash --login -c \\\"CIRCA_MM_HOST=$mm_host CIRCA_HOST=$newhost CIRCA_PORT=$fuzzbomb_circa_port \\\\\\\${DOCKER_LACROSSE_HOME}/code/tools/lax-run-sbcl-agent $args\\\"' > $log_path 2>&1 &\n";

    $sub .= "$cgc/rsync/code/tools/sshc $mm_host '$cgc/rsync/code/tools/docker exec $basename-neo-fuzz-ccl /realuser.sh bash --login -c \\\"CIRCA_MM_HOST=$mm_host CIRCA_HOST=$newhost CIRCA_PORT=$fuzzbomb_circa_port \\\\\\\${DOCKER_LACROSSE_HOME}/code/tools/send-telemetry-logfile.py $name $inner_log_path\\\"' > $send_telem_path 2>&1 &\n";

    ++$fbs_in_slot;
    ++$ampcount;
  }
}

# AFAICT we no longer use the doit.sh and we encounter an error trying to write it, so I am removing it.
# Using POD block since perl doesn't have block comments. 

# retrofitting this stuff to the start of the doit.sub file string, to start docker containers once per used host
# note evil '--network host' so that container has host's hostname, for MM connections.  Port deconfliction via CIRCA_BASEPORT should be good enuf
# note that --network option now moved to start-neo-fuzz-ccl so always get it

# note that while we need to use the rsync/code at the host level, when we get inside the container
# the /neo-fuzz root is mounted to the rsync automatically, so no need to think about rsync level (and in fact if you add it, bad)
my $starts = "# ssh launcher for job $main::hostname:$jobdir\n";

# first make sure each host has rootless docker server started.... NOTE FIXME this should really be conditional on
# whether the system has rooted docker installed, but at this point SIFT has none of those so... imposs to test that.

foreach my $host (keys(%used_hosts)) {
  if (!($host eq $main::hostname)) {
    #$starts .= "$cgc/code/tools/sshc $host $cgc/rsync/code/tools/start-docker-engine > start-docker-engine-$host.log 2>&1\n";
    $starts .= "$cgc/rsync/code/tools/sshc $host $cgc/rsync/code/tools/start-docker-engine > start-docker-engine-$host.log 2>&1\n";

  }}

warn("1cgc: $cgc\n");
warn("main::hostname: $main::hostname\n");
warn("main::cgc: $main::cgc\n");
foreach my $host (keys(%used_hosts)) {
  # if ($host eq $main::hostname && defined($ENV{LOOPBACK})) {
  #   $mm_host = $main::loopback;
  # } else {
  #   $mm_host = $main::hostname;
  # }
  $mm_host = $main::hostname;

  my @vars = (\$starts, \$cgc, \$host, \$basename, \$baseport, \$mm_host, \$docker_host, \$docker_tag);
  my @varnames = ("$starts", "$cgc", "$host", "$basename", "$baseport", "$mm_host", "$docker_host", "$docker_tag");
  die unless $#vars == $#varnames;
  for (my $i = 0; $i <= $#vars; $i++) {
    if (${$vars[$i]} eq '') {
      die "Variable $varnames[$i] used in building ssh launcher is undefined.";
    }
  }
  my @keys = ('OPENAI_API_KEY');
  for (my $i = 0; $i <= $#keys; $i++) {
    if ($ENV{$keys[$i]} eq '') {
      die "ENV entry $keys[$i] used in building ssh launcher is undefined.";
    }
  }
  $starts .= "$cgc/rsync/code/tools/sshc $host CIRCA_BASENAME=$basename CIRCA_BASEPORT=$baseport CIRCA_MM_HOST=$mm_host DOCKER_HOST=$docker_host DOCKER_TAG=$docker_tag OPENAI_API_KEY=$ENV{'OPENAI_API_KEY'} HOST_LOGDIR=$jobdir $cgc/rsync/code/tools/start-neo-fuzz-sbcl -d > start-neo-fuzz-sbcl-${host}.log 2>&1\n";

}

#my $reboot="";

warn("2cgc: $cgc\n");
warn("main::hostname: $main::hostname\n");
warn("main::cgc: $main::cgc\n");
foreach my $host (keys(%used_hosts)) {
  #$starts .= "$cgc/code/tools/sshc $host DOCKER_HOST=$docker_host $cgc/rsync/code/tools/wait-for-container $basename-neo-fuzz-ccl > wait-for-neo-fuzz-ccl-$host.log 2>&1 &\n";
## NOTE do *not* background wait-for tasks....they must finish before next steps
  $starts .= "$cgc/rsync/code/tools/sshc $host DOCKER_HOST=$docker_host $cgc/rsync/code/tools/wait-for-container $basename-neo-fuzz-ccl > wait-for-neo-fuzz-ccl-$host.log 2>&1 &\n";
  #$starts .= "$main::cgc/code/tools/sshc $host DOCKER_HOST=$main::docker_host $main::cgc/rsync/code/tools/with-tool-path wait-for-container $basename-neo-fuzz-ccl > wait-for-neo-fuzz-ccl-$host.log 2>&1 \n";

  # now start the dinds reqd for the fuzzing of Finals tasks
  $starts .= "$cgc/rsync/code/tools/sshc $host DOCKER_HOST=$docker_host $cgc/rsync/code/tools/start-dind > start-dind-$host.log 2>&1 &\n";
  $starts .= "$cgc/rsync/code/prt/timestamp $cgc/rsync/code/tools/monitor-dind.sh $host > monitor-dind-$host.log 2>&1 &\n";
  # FIXME should add a wait/logwatcher for the dind to be up...but it just better be...

  # write a dind rebooter for each host
  # alternatively, could have just tools/reboot-dind $host scripting but wouldnt know where to send the logs...
  # note we append to the rebooting log so can see if it happened multiple times
#  $reboot = "$cgc/rsync/code/prt/timestamp echo Rebooting dind for $host >> rebooted-dind-$host.log 2>&1 \n";
#  $reboot .= "$cgc/rsync/code/prt/timestamp $cgc/rsync/code/tools/sshc $host docker rm -f $ENV{'USER'}-dind >> rebooted-dind-$host.log 2>&1 \n";
#  $reboot .= "$cgc/rsync/code/prt/timestamp $cgc/rsync/code/tools/sshc $host DOCKER_HOST=$docker_host $cgc/rsync/code/tools/start-dind >> rebooted-dind-$host.log 2>&1 &\n";
#  write_filestring("$jobdir/reboot-dind-$host.sh", $reboot);
#  chmod 0755, "$jobdir/reboot-dind-$host.sh";
}

# now we add a wait to make sure all those wait-for-containers have finished
#$sub = $starts . "wait\n" . $sub;
#$sub = $starts . "wait\n echo Finished wait\n" . $sub;
#$sub = $starts . "echo Finished wait-for-ctr; sleep 1; echo Finished sleep\n" . $sub;


#my $log_path="$jobdir/start-dind.log";
#my $dind = join(" ",
#               #"$cgc/code/tools/sshc",
#               #$newhost,
#               "$cgc/code/tools/docker exec $basename-neo-fuzz-ccl /realuser.sh bash --login -c \\\"/lacrosse/code/tools/start-dind\\\"",
#               "> $log_path 2>&1 &\n");
#$sub = $starts . "echo Finished wait-for-ctr\n" . $dind . $sub;
$sub = "#!/bin/bash\n" . $starts . "echo Finished wait-for-ctr\n" . $sub;

my $submitfile = "$jobdir/doit.sh";
write_filestring($submitfile, $sub);
warn("ssh driver file written to $submitfile\n");
chdir($jobdir);
chmod 0755, "./doit.sh";
my $cmd = "./doit.sh";
if ($justdontdoit) {
  warn("NOT Running $cmd\n");
} else {
  warn("Running $cmd\n");
  my $results = `$cmd`;
}
#return($cluster);
