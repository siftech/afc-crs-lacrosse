### This function is a wrapper around Symbol::gensym.  Instead of returning
### an unreadable GLOB, it returns a readable string.  So, for example, instead of
### returning GLOB(0x559b9a755d28), which is ugly when printed, e.g.:
### Unable to find pid for agent 'echo-GLOB(0x559b9a755d28)'
### you get a string like "GEN0" which gives you 
### Unable to find pid for agent 'echo-GEN0' which doesn't look like there's
### a bug in the naming code!  
sub prtGensym {
  require Symbol;
  my $sym = Symbol::gensym;
  my $mysym = ${$sym};
  $mysym =~ m|.*::(.*)|;
  return $1;
}

sub echo {	# echoes a string to console as test is run; if nonzero third arg, prints newline first (useful for first echo in a test); leaves no logfile
		# new: you can now call this with one string and it will gensym the agent name; if the second arg looks like a number it will treat the first
		#	arg as the string to print and turn on the startingnewline.

    # use Symbol qw(gensym);
    use Scalar::Util qw(looks_like_number);
    my ($name,$string,$startingnewline) = @_;
    if (not (defined($string))) { $string=$name; $name=prtGensym();}
    elsif ((not (defined($startingnewline))) && looks_like_number($string)) { $string=$name; $name=gensym; $startingnewline=1;}
    my @agent = ({
        name      => "echo-$name",
        command   => sub {
            use POSIX qw(strftime);
            $now_string = strftime "%F %T", localtime;
            if ($startingnewline) { print "\n";}
            print "$now_string $string\n";
            exit 0; },
        complete_before_continuing => 1,
        comparo => undef,
                 },);
    return @agent;
}

sub rm {	# removes given filename from the test's results dir; leaves no logfile
    my ($filename) = @_;
    my @agent = ({
        name      => "rm-$filename",
        command   => sub { my $agent=shift; @files= glob $agent->testspec->results_dir . "/$filename"; 
				#print "Removing $files[0]\n"; 
				unlink $files[0]; exit 0; },
        complete_before_continuing => 1,
        comparo => undef,
                 },);
    return @agent;
}

sub cp {	# copies given file from test dir (or whereever, if a relative or full path) to the test's results dir; leaves no logfile
		# new: can take second arg as destination
    my ($filename,$dest) = @_;
    my @agent = ({
        name      => "cp-$filename",
        command   => sub { use File::Copy qw(copy); my $agent=shift; @files= glob $agent->testspec->results_dir; 
				if (not (defined $dest)) { $dest = $files[0]; }
				#print "Copying $filename to $dest\n"; 
				exec 'bash', '-c', "(cp $filename $dest)"; },
        complete_before_continuing => 1,
        comparo => undef,
                 },);
    return @agent;
}

sub mv {	# moves given file from test dir (or whereever, if a relative or full path) to the test's results dir; leaves no logfile
		# new: can take second arg as destination
    my ($filename,$dest) = @_;
    my @agent = ({
        name      => "mv-$filename",
        command   => sub { use File::Copy qw(copy); my $agent=shift; @files= glob $agent->testspec->results_dir; 
				if (not (defined $dest)) { $dest = $files[0]; }
				#print "Moving $filename to $dest\n"; 
				exec 'bash', '-c', "(mv $filename $dest)"; },
        complete_before_continuing => 1,
        comparo => undef,
                 },);
    return @agent;
}

sub sh {	# runs arbitrary shell cmd in test's results dir; leaves no logfile
		# if you want to run a cmd in a different directory, use cd-then-exec
    my $args=join(' ',@_);
    # use Symbol qw(gensym);
    my @agent = ({
        name      => prtGensym(),
        command   => sub { #warn "execing [$args]\n";
			   exec $args; },
        complete_before_continuing => 1,
        comparo => undef, },);
    return @agent;
}

sub delay {
    my ($seconds) = @_;
    return {
          name => "delay",
          description => "Delay specified seconds before continuing PRT",
          command => sub {
            use POSIX qw(strftime);
            $now_string = strftime "%F %T", localtime;
            print "$now_string Waiting for $seconds seconds\n";
            sleep($seconds);
            exit 0;
          },
          comparo => undef,
          complete_before_continuing => 1,
      },
}

sub printenv { 	# print all the env vars to a logfile for help debugging issues; note you must call this as printenv(), in the agent list, not just printenv,
    my @agent = ({
    	name => "printenv",
    	description => "print the env vars to a logfile for help debugging issues",
    	command   => ['perl', '-e', 'foreach my $var (keys(%ENV)) { print ("$var=$ENV{$var}\n");}; exit 0'],
        comparo => undef, },);
    return @agent;
}

sub print_results_dir {
    my @agent = ({
        name => "print-results-dir",
        command => sub { my $agent = shift; print "\nResults: " . $agent->testspec->results_dir . "\n"; exit 0;},
        complete_before_continuing => 1,
        comparo => undef, },);
    return @agent;
    }

1; 	# Return something for "do"!
