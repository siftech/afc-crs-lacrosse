# common agents shared by most/all condor cluster tests

#use PRT::Perf qw(smaller_is_better bigger_is_better
#                 numeric_in_range same_number);

do "../prt/common.pl" || die "Can't do '../prt/common.pl': $@";

# What is the name of the script to use to start neo-fuzz running?
sub nf_launcher() {
  if ($use_ccl_in_docker) {
    if ($use_sbcl) {
      return "../tools/neofuzz-sbcl-launcher.pl";
    } elsif ($use_ccl) {
      return "../tools/neofuzz-ccl-launcher.pl"; 	 # use CCL in docker container, for deliverability
    }
    else {
      die "Exactly one of \$use_ccl or \$use_sbcl must be true.";
    }
  } else {
    return "../tools/neofuzz-launcher.pl";
  }
};

# What is the compile command we should use to build the lisp code for FB?
sub compile_command () {
  if ($use_ccl_in_docker) {
    die "Must specify exactly one of CCL or SBCL" if ( $use_ccl && $use_sbcl ) or not ( $use_ccl || $use_sbcl);
    if ($use_ccl) {
      return "../tools/compile-ccl-in-docker";
    } elsif ($use_sbcl) {
      return "../tools/compile-sbcl-in-docker";
    }
  } else {
    return $compile_lisp;
  }
};

sub setenv_sub() {
    # Check for required vars.
    foreach my $var (qw(CIRCA_BASEPORT CIRCA_BASENAME DOCKER_SUBNET )) {
        unless (defined $ENV{$var}) {
            die "Need to define $var in environment; failing";
        }
    }

    # Set derived values, eg, paths that depend on an env var, here.
};

our @setenv = (
    {
        name => "setenv",
        description => "Check env vars; set any derived env vars.",
        command => ['perl', '-e', 'do "./common.pl"; setenv_sub();'],
        comparo => undef,
    },
    );

our @cluster_stats= (
    {
        name => "cluster-loads",
        command => ['../tools/cluster-loads'],
        complete_before_continuing  => 1,
        exit_code             => undef,
        comparo => undef,
    },
    {
        name => "cluster-allocation",
        command => ['cat', '/tmp/cluster-allocation'],
        complete_before_continuing  => 1,
        exit_code             => undef,
        comparo => undef,
    },
    );

		#"/usr/bin/xterm", "-hold", "-e", "tail", "-f", "-s", "5", "$_[0]" ],
	#"/usr/bin/gnome-terminal",  "--", "tail", "-f", "-s", "5", "$_[0]" ],
sub xterm_tail() {
    (echo("Opening xterm window for $_[0]"),
    {
        name                => "xterm-$_[1]",
        command             => [ "cd-then-exec", sub {my $agent=shift; return $agent->testspec->results_dir;},
        "/usr/bin/xterm", "-geometry", "75x24+0+0","-fa","Monospace","-fs","18","-hold", "-e", "tail","-f","-n","+1","-s","5", "$_[0]"],
        comparo             => undef,
        exit_code             => undef,
	daemon =>1,	# with this, test can do xterm and will kill it when test done
        complete_before_continuing  => 0,
    }
    )};

our @kill_stray_local_processes = (
    echo("Starting kill-stray-local-processes"),
    {
        name                        => "kill-stray-local-processes",
        command                     => [ "../tools/kill-stray-local-processes" ],
        comparo                     => undef,
        exit_code                   => undef,
        complete_before_continuing  => 1,
    },
    echo("Finished kill-stray-local-processes"),
    );

our @ensure_mm = (
	{
	    name => "ensure-new-matchmaker",
	    description => "Make sure the matchmaker is running",
	    command      => [ "../matchmaker/lax-ensure-new-matchmaker" ],
            complete_before_continuing =>1,
	    comparo => undef,
	},
     );

our @siftfield = (
      	echo("Starting siftfield."),
        {
	    name         => "siftfield",
	    description  => "Runs siftfield (requires X display)",
	    command      => [ "sift-field.py",
                              "--log-type", "neo",
                              "--cell-width", "18",
                              "--cell-height", "18",
                              "--align-time",
                              "--auto-scroll",
                              "--monitor", sub {my $agent=shift; return $agent->testspec->results_dir . "/";},
                            ],
  	    comparo      => undef,
            exit_code    => undef,
	    complete_before_continuing => 0,
	    daemon => $siftfield_isa_daemon,
        },
);

# this below is OBE by defaults.pl; saving for one more commit for posterity
#if (!(defined($net))) { our $net = ""; } 	# define this before 'do'ing this file if you want to send in a docker network - see demo-2018*
## NOTE if an earlier test defines this variable, it will remain defined in later tests.  Bummer.  So it may in fact be better
## to not use this if(!defined) pattern but instead set them to a default value at the top of common.pl or some earlier always-used defaults.pl and
## then rely on that always resetting them at the start of each new test, before any possible test-specific values.

# this is for starting driller on localhost for single-host tests.  see just-optimus*
our @start_driller = (
        echo("Starting driller container"),
        {
            name         => "driller",
            command      => [ "../tools/start-driller", "-d", "$net" ],
            comparo      => undef,
            complete_before_continuing =>1,
        },
        echo("Waiting for driller container"),
        {
            name         => "wait_for_driller",
            description  => "watches to make sure driller gets up",
            command      => [ "../prt/logwatcher.pl", "--dockername", "$ENV{'USER'}-driller",
			        "-p", "container is up",
        			"-p", "Driller docker is up",
        			"-p", "driller docker image already running",
        			"-q", "error",
        			"-q", "Error"
				],
            comparo      => undef,
            complete_before_continuing => 1,
        },
        $driller_must_survive ? echo("Starting daemon to make sure driller container stays up") : (),
        $driller_must_survive ?
        {
            name         => "driller_stays_alive",
            description  => "watches to make sure driller stays alive",
            command      => [ "../tools/docker-stays-alive", "$ENV{'USER'}-driller",
                ],
            comparo      => undef,
            daemon =>1,
            must_survive => $driller_must_survive,
            exit_code    => undef,
            complete_before_continuing =>0,
        } : (),
  );

# this is for starting afl-qemu on localhost for single-host tests.  see just-optimus*
our @start_afl_qemu = (
        echo("Starting afl-qemu"),
        {
            name         => "afl_qemu",
            command      => [ "../tools/start-afl-qemu", "-d" ],
            comparo      => undef,
            complete_before_continuing =>1,
        },
        echo("Waiting for afl-qemu"),
        {
            name         => "wait_for_afl_qemu",
            command      => [ "../prt/logwatcher.pl",
                              "--dockername", "$ENV{'USER'}-afl-qemu",
                              # Success! - stop if you see one of these
                              "-p", "start-afl-qemu is returning",
                              "-p", "afl-qemu docker image already running",
                              # ERRORs - stop if you see one of these
                              "-q", "error",
                              "-q", "Error",
                ],
            comparo      => undef,
            complete_before_continuing => 1,
        },
        echo("Starting daemon to make sure afl-qemu stays up"),
        {
            name         => "afl_qemu_stays_alive",
            description  => "watches to make sure afl-qemu stays alive",
            command      => [ "../tools/docker-stays-alive", "$ENV{'USER'}-afl-qemu",
                ],
            comparo      => undef,
            daemon =>1,
            must_survive => 1,
            exit_code    => undef,
            complete_before_continuing =>0,
        },
  );

our @start_fuzzball = (
        echo("Starting fuzzball container"),
        {
            name         => "fuzzball",
            command      => [ "../tools/start-fuzzball", "-d", "$net" ],
            comparo      => undef,
            complete_before_continuing =>1,
        },
        echo("Waiting for fuzzball container"),
        {
            name         => "wait_for_fuzzball",
            description  => "watches to make sure fuzzball gets up",
            command      => [ "../prt/logwatcher.pl", "--dockername", "$ENV{'USER'}-fuzzball",
        			"-p", "container is up",
        			"-p", "container already running",
        			"-q", "error",
        			"-q", "Error"
				],
            comparo      => undef,
            complete_before_continuing => 1,
        },
        $fuzzball_must_survive ? echo("Starting daemon to make sure fuzzball container stays up") : (),
        $fuzzball_must_survive ?
        {
            name         => "fuzzball_stays_alive",
            description  => "watches to make sure fuzzball stays alive",
            command      => [ "../tools/docker-stays-alive", "$ENV{'USER'}-fuzzball",
                ],
            comparo      => undef,
            daemon =>1,
            must_survive => $fuzzball_must_survive,
            exit_code    => undef,
            complete_before_continuing =>0,
        } : (),
  );
# [pwalker:20180830.1017CDT] This _SHOULD_ make all the following kill_* agents obsolete
our @kill_my_dockers = (
    echo("Killing $ENV{'USER'}'s docker containers"),
    {
        name        => "killing_my_dockers",
        command     => [ "../tools/kill-my-dockers" ],
        comparo     => undef,
        complete_before_continuing  => 1,
        exit_code   => undef,
    }
    );

sub kill_user_docker {
    my ($docker_suffix, $agent_suffix_arg) = @_;
    my $agent_suffix = defined $agent_suffix_arg ? "-${agent_suffix_arg}" : "";
    my $echo_suffix = defined $agent_suffix_arg ? " (${agent_suffix_arg})" : "";
    my @agent = (
        echo("Killing $ENV{'USER'}-${docker_suffix}${echo_suffix}"),
        {
            name => "kill-$ENV{'USER'}-${docker_suffix}${agent_suffix}",
            command => [ "docker", "rm", "-f", "$ENV{'USER'}-${docker_suffix}"],
            comparo      => undef,
            complete_before_continuing =>1,
            exit_code    => undef,
        },
        echo("Finished killing $ENV{'USER'}-${docker_suffix}${echo_suffix}"),
        );
    return @agent;
}

our @shutdown_optimus = (
    echo("Shutting down Optimus"),
    {
        name         => "shutdown-optimus",
        description  => "tells optimus to shut everyone down",
        command      => [ "../matchmaker/tell-amp",
                          "-n", sub {return "OPTIMUS0-AMP-$ENV{USER}-ACCEPTOR";},
                          "-k",
            ],
        comparo      => undef,
        complete_before_continuing => 1,
    },
    );

our @kill_docker_net = (
    {
        name         => "kill_docker_net",
        command      => [ "docker", "network", "rm", "$ENV{'USER'}-docker-net"],
        comparo      => undef,
        complete_before_continuing =>1,
        exit_code    => undef,
    }
    );

our @start_gui = (
	#echo("Updating or installing GUI dependencies"),
	#{
	#name => "waiting_for_gui_install",
	#command => [ "../tools/install-gui-deps" ],
	##comparo => undef,
	#complete_before_continuing => 1,
	#},
    echo("Waiting for AMP socket before starting GUI"),
    {
	    name         => "wait_for_amp_socket",
	    command      => [ "../prt/logwatcher.pl",
			      "-f", sub { my $agent=shift; @files= glob $agent->testspec->results_dir . "/OPTIMUS0.log"; return $files[0]; },
			      # Success! - stop if you see one of these
                              "-p", "Disconnecting from matchmaker",
			      # ERRORs - stop if you see one of these
			      "-q", "An unhandled error occurred during initialization",
			      "-q", "An error occurred",
			      "-q", "Received signal number",
			      "-q", "carry on...",
			      "-q", "ABORT",		# this is a thing AFL sez
			      "-q", "abort",
			      "-q", "; Exiting",
		],
  	    comparo      => undef,
	    complete_before_continuing => 1,
	},
    echo("Starting GUI"),
    {
	name => "gui",
	command => [ "../tools/start-gui" ],
	comparo => undef,
	complete_before_continuing => 0,
	#daemon => 1,
    },
    echo("Waiting for GUI up"),
	{
	    name         => "wait_for_gui_up",
	    command      => [ "../prt/logwatcher.pl",
			      "-f", sub { my $agent=shift; @files= glob $agent->testspec->results_dir . "/gui.log"; return $files[0]; },
			      # Success! - stop if you see one of these
                              "-p", "GUI is up",
			      # ERRORs - stop if you see one of these
			      "-q", "abort",
		],
  	    comparo      => undef,
	    complete_before_continuing => 1,
	},
);

our @make_demo_sys = (
	echo("Making demo_sys "),
	{
	    name                        => "make-demo-sys",
	    command                     => [ "../prt/cd-then-exec","../targets/demo_sys","/usr/bin/make" ],
	    complete_before_continuing  => 1,
        exit_code                   => undef,
        comparo                     => undef,
	},
 );

our @start_demo_sys = (
	echo("Starting kill-stray-local-processes"),
	{
	    name                        => "kill-stray-local-processes",
	    command                     => [ "../tools/kill-stray-local-processes" ],
  	    comparo                     => undef,
       	    exit_code                   => undef,
	    complete_before_continuing  => 1,
	},
	echo("Finished kill-stray-local-processes"),
    @kill_my_dockers,
    echo("Starting demo_sys network"),
    {
        name                        => "docker-net",
        command                     => "../tools/start-docker-net",
        comparo                     => undef,
        complete_before_continuing  => 1,
    }, echo("Starting demo_sys router container"),
    {
        name                        => "demo-router-container",
        command                     => "../tools/start-demo-router",
        comparo                     => undef,
        daemon                      => 0,
        exit_code                   => undef,
        complete_before_continuing  => 1,
    },
    echo("Running demo_sys router"),
    {
        name                        => "demo-router-process",
        command                     => "../tools/run-demo-router",
        comparo                     => undef,
        daemon                      => 0,
        exit_code                   => undef,
        complete_before_continuing  => 0,
    },
    echo("Starting demo_sys IMU container"),
    {
        name                        => "demo-imu-container",
        command                     => "../tools/start-demo-imu",
        comparo                     => undef,
        daemon                      => 0,
        exit_code                   => undef,
        complete_before_continuing  => 1,
    },
    echo("Running demo_sys IMU"),
    {
        name                        => "demo-imu-process",
        command                     => "../tools/run-demo-imu",
        comparo                     => undef,
        daemon                      => 0,
        exit_code                   => undef,
        complete_before_continuing  => 0,
    },
    echo("Starting demo_sys display container"),
    {
        name                        => "demo-display-container",
        command                     => "../tools/start-demo-display",
        comparo                     => undef,
        daemon                      => 0,
        exit_code                   =>undef,
        complete_before_continuing  => 1,
    },
    echo("Running demo_sys display"),
    {
        name                        => "demo-display-process",
        command                     => "../tools/run-demo-display",
        comparo                     => undef,
        daemon                      => 0,
        exit_code                   => undef,
        complete_before_continuing  => 0,
    },
#        echo("Watching demo_sys display"),
#        {
#            name                        => "demo_display_watch",
#            command                     => ["xterm", "-hold", "-e", "tail", "-f", "-s", "5", sub {my $agent=shift; @files= glob $agent->testspec->results_dir . "/demo_display.log"; return $files[0];}],
#            comparo                     => 0,
#            complete_before_continuing  => 0,
#        },
    );

our @start_docker_net = (
    echo("Starting docker net"),
    {
        name                        => "docker-net",
        command                     => "../tools/start-docker-net",
        comparo                     => undef,
        complete_before_continuing  => 1,
    },
    );

our @proto = (
    echo("Starting proto"),
    {
        name        => "proto",
        description => "Ensure protocol learning works correctly",
        command     => ['../tools/test-proto'],
        comparo     => new PRT::LineComparo(
                            key_fact_regex => "RESULT:.*",
                    ),
        complete_before_continuing  => 1,
    },
);

# Ok this lisp compilation stuff actually swings three ways: either ACL (default) on localhost, CCL on localhost if
# PRT_LISP is set to 'ccl', and if a test sets $use_ccl_in_docker, guess what?

my $compile_lisp = [ '../tools/exec-test-form-in-lisp.pl',
                          '--run_dir', '../lisp',
                          '--test_form', '(progn (pushnew :sift-developer *features*)
                                                 (pushnew :regress *features*)
                                                 (load "load.lisp")
						 (musliner::dbug :top "printenv: ~s" (uiop:run-program "printenv" :output :string))
                                                 )'
            ];

if (defined $ENV{'PRT_LISP'}) {
    my $prt_lisp = $ENV{'PRT_LISP'};
    push @$compile_lisp, "--lisp", "$prt_lisp";
}


our @compile_lisp = (
  echo("Starting Lisp compile"),
  {
    ## compile lisp and make sure it works
    name         => "compile-lisp",
    command      => [ "../tools/compile-sbcl-in-docker", sub { my $agent=shift; $dir=$agent->testspec->results_dir; return("$dir"); } ],
    comparo      => undef,
    complete_before_continuing => 1,
  },
  echo("Lisp compile has completed"),
    );


our @compile_mm_within_docker = (
  echo("Starting Matchmaker compile"),
  {
    ## compile matchmaker and make sure it works
    name         => "compile-matchmaker",
    command      => [ "../tools/within-docker-compile-matchmaker" ],
    comparo      => undef,
    complete_before_continuing => 1,
  },
  echo("Matchmaker compile has completed"),
    );


our @compile_whatclib = (
	echo("Starting to compile whatclib"),
	{ 	## compile the rust
		name         => "compile-whatclib",
		command      => ["../tools/compile-whatclib"],
  	    comparo      => undef,
	    complete_before_continuing => 1,
	},
	echo("Completed compiling whatclib"),
);

our @compile_qemu = (
	echo("Starting to compile QEMU"),
	{
	    name         => "compile-qemu",
	    command      => ["../tools/qemu-arm-build"],
  	    comparo      => undef,
	    complete_before_continuing => 1,
	},
	echo("Completed compiling QEMU"),
);

our @compile_aflplusplus_hb = (
	echo("Starting to compile AFLPlusPlus-HB"),
	{
	    name         => "compile-aflplusplus-hb",
	    command      => ["../tools/aflplusplus-hb-build"],
  	    comparo      => undef,
	    complete_before_continuing => 1,
	},
	echo("Completed compiling AFLPlusPlus-HB"),
);

our @compile_fuzzball = (
    echo("Starting to compile fuzzball"),
    {
        name         => "compile-fuzzball",
        command      => ["../tools/compile-fuzzball"],
  	comparo      => undef,
        complete_before_continuing => 1,
    },
    echo("Completed compiling fuzzball"),
);



our @rsync_code = (
	echo("Starting rsync-code"),

	{	## start the rsync early and below use logwatcher to make sure it is finished OK before run any code
		## Note that doit does another rsync-code later, after the code/experiment.lisp file is created, but that should be the only changed file and
		## so it is fast.  We use this prt-level agent here to detect rsync problems a bit earlier and more obviously.
	    name         => "rsync-code",
	    command      => [ "../tools/rsync-code", "$localhostonly" ],
  	    comparo      => undef,
	    complete_before_continuing => 0,
	},

#	{
#	    name         => "crma-if-wonky",
#	    command      => [ "../tools/crma-if-wonky" ],
#  	    comparo      => undef,
#	    complete_before_continuing => 1,
#	},
	{
	    name         => "ensure-rsync-complete",
	    command      => [ "../prt/logwatcher.pl",
			      "-f", sub { my $agent=shift; @files= glob $agent->testspec->results_dir . "/rsync-code.log"; return $files[0]; },
			      	# Success! - stop if you see one of these
                              "-p", "rsync-code complete",
			      	# ERRORs - stop if you see one of these
			      "-q", "rsync failed",
		],
  	    comparo      => undef,
	    complete_before_continuing => 1,
	},
	echo("rsync-code has completed"),
  );

# set $success_pattern if you want the standard checking of OPTIMUS0.log for that pattern
# set $success_logfile to override which logfile is examined by default
our @success_agents = (
	echo("Waiting up to $success_timeout for success pattern [$success_pattern] in $success_logfile"),
	{
	    name         => "wait_for_optimus",
	    #command      => [ "timeout", "10m","../prt/logwatcher.pl",   # timeout should be a variable!  Relate to the halt msg timeout? nontriv
	    command      => [ "timeout", $success_timeout, "../prt/logwatcher.pl",
			      "-f", sub { my $agent=shift; @files= glob $agent->testspec->results_dir . "/$success_logfile"; return $files[0]; },
			      # Success! - stop if you see one of these
                              "-p", $success_pattern,
                              "-q", $failure_pattern,
			      "-s", $sleeptime,
			      # ERRORs - stop if you see one of these
			      #"-q", "An unhandled error occurred during initialization",
			      #"-q", "An error occurred",
			      #"-q", "Feature NOT licensed",
			      #"-q", "Received signal number",
			      #"-q", "carry on...",
			      #"-q", "; Exiting",
		],
  	    comparo      => undef,
	    complete_before_continuing => 1,
	},
	echo("Success condition found"),
);

our @local_success_agents = (
	echo("Waiting for success pattern [$local_success_pattern] in $success_logfile"),
	{
	    name         => "wait_for_optimus",
	    #command      => [ "timeout", "10m","../prt/logwatcher.pl",   # timeout should be a variable!  Relate to the halt msg timeout? nontriv
	    command      => [ "../prt/logwatcher.pl",
			      "-f", sub { my $agent=shift; @files= glob $agent->testspec->results_dir . "/$success_logfile"; return $files[0]; },
			      # Success! - stop if you see one of these
                              "-p", $local_success_pattern,
			      "-s", $sleeptime,
			      # ERRORs - stop if you see one of these
			      "-q", "An unhandled error occurred during initialization",
			      "-q", "An error occurred",
			      "-q", "Feature NOT licensed",
			      "-q", "Received signal number",
			      "-q", "carry on...",
			      #"-q", "ABORT",		# this is a thing AFL sez; as of Feb 19 2018 we can survive/use this
			      "-q", "\w*(?<!calls )abort",
			      "-q", "; Exiting",
                              "-q", "ERROR:", # CCL error
                              "-q", "Traceback", # python error
		],
  	    comparo      => undef,
	    complete_before_continuing => 1,
	},
	echo("Success condition found"),
);


our @optimus_comparo_agent = (
    {
        name                => "optimus-comparo-agent",
	#logfile	=> sub {my $agent=shift; return $agent->testspec->results_dir . "/OPTIMUS0.log";},
        command             => [ "grep", $optimus_comparo_regexp,
                                 sub {my $agent=shift; return($agent->testspec->results_dir . "/$optimus_comparo_logfile");},],
        #command             => [ "true"],
        comparo      => $optimus_comparo,
	#perfchecks => $optimus_perfchecks,
        complete_before_continuing  => 1,
    }
);

our @compose_agents = (
        {
		# this writes a custom compose.yaml for how many OPT/FBs you want
            name         => "create-compose",
            #command      => [ "../tools/dupl-template", "<../crs-sandbox-items/template-for-compose", ">../../../compose.yaml" ],
            command      => [ "../tools/run-dupl-template","1", $numfbs, "$use_sbcl"],       # first arg sez atsift
            comparo      => undef,
            complete_before_continuing => 1,
        } ,
		# this writes a custom experiment.lisp for how many OPT/FBs you want...and doesnt run doit.sh
        {
            name         => "neofuzz-launcher",
            command      => [ nf_launcher(),
                                sub { my $agent=shift; return $agent->testspec->results_dir;},  # the test results dir
                                $experiment, "$numoptimi", "$numfbs", "$localhostonly", "$usegui", "$allocationmethod", "justdontdoit"
                              ],
            comparo      => undef,
            complete_before_continuing => 1,
        },
	{
	    name	=> "docker-compose-pull",
	    command	=> [ "docker", "compose", "--profile", "development", "pull", "--ignore-buildable", "--policy", "missing" ],
	    comparo	=> undef,
	    complete_before_continuing	=> 1,
	},
    {
        name         => "copy-compose-agents-config",
        command      => [ "../tools/copy-compose-agents-config", sub {my $agent=shift; return ($agent->testspec->results_dir);} ],
        comparo      => undef,
        complete_before_continuing => 1,
    },
	{
		# build the images (with compose)
	    name         => "ensure-local-dev-images",
	    command      => [ "../tools/ensure-local-dev-images"],
	    comparo      => undef,
	    complete_before_continuing => 1,
	},
	{
		# build the images (with compose)
	    name         => "make-build",
	    command      => [ "../prt/cd-then-exec", "../../../", "/bin/make", "build"],
	    comparo      => undef,
	    complete_before_continuing => 1,
	},
	{
		# prune all the dangling images created by the compose build (one per fuzzbomb after the first)
	    name         => "docker-image-prune",
	    command      => [ "../tools/docker", "image", "prune" ],
	    comparo      => undef,
	    complete_before_continuing => 1,
	},
	{
		# stop any already running services that are associated with the compose file
	    name         => "make-down",
	    command      => [ "../prt/cd-then-exec", "../../../", "/bin/make", "down" ],
	    comparo      => undef,
	    complete_before_continuing => 1,
	},
	{
		# start any services that are associated with the compose file
	    name         => "make-up",
	    command      => [ "../prt/cd-then-exec", "../../../", "/bin/make", "up"],
	    comparo      => undef,
	    complete_before_continuing => 1,
	}
    );

our @semi_get_cp = (
    echo("Getting the CP"),
    {
        name         => "set-CP",
        description  => "Sets the CP for agents to think about",
        command      => [ "cp","-v","$cp_config","../../../cp_config/cp_config.yaml" ],
        comparo      => undef,
        complete_before_continuing => 1,
    },
    {
        name         => "get-CP",
        description  => "Gets the CP for agents to think about",
        command      => [ "../prt/cd-then-exec", "../../../", "/bin/make","cps" ],
        comparo      => undef,
        complete_before_continuing => 1,
    }
    );

our @get_final_smoketest_task = (
    echo("Getting the Final Smoketest Task."),
    {
        name         => "get-final-smoketest-task",
        command      => ["../tools/host-afc-populate-cp-root", $task_id, $url1, $url2, $url3 ],
        comparo      => undef,
        complete_before_continuing =>1,
    }
    );

# to emulate real competition environment, we must put this test's single CP into someplace mounted into ${AIXCC_CP_ROOT} and
# expect the CRS to copy it to /crs_scratch.  The small challenge, then, is to decide where ${AIXCC_CP_ROOT} should be and how
# to get it mounted into the docker-compose-started agents. Maybe the local mods? or extra-args...
# Actually, no, I think we just need to get cp_config.yaml to only list the right target and then do 'make cps'
our @before_lisp_agents = (
    $run_in_compose ?
        (
            echo("Cleaning out any old CP stuff"),
                {
                    name         => "clean-CP",
                    command      => [ "../prt/cd-then-exec", "../../../", "/bin/make","cps/clean" ],
                    comparo      => undef,
                    complete_before_continuing => 1,
                },
            echo("Setting up Google/VertexAI credentials"),
                {
                    name         => "copy-vertex-key",
                    description  => "Copy the vertex_key.json file we need.",
                    command      => [ "cp", "../crs-sandbox-items/competition/lax_vertex_key.json", "../../../sandbox/litellm/vertex_key.json" ],
                    comparo      => undef,
                    complete_before_continuing => 1,
                },
        ) : (),

    $final_smoketest ?
      @get_final_smoketest_task :
      (),
    );

our @trigger_example_competition_server_to_task = (
    echo("Triggering competition server to task LAX CRS"),
    {
        name         => "trigger-comp-server-to-task",
        command      => [ "../tools/example-competition-server/trigger-comp-server-to-task", $task_script ],
        comparo      => undef,
	    exit_code    => undef,	# this b/c is daemon and will die badly at end of test
        complete_before_continuing =>0,
    },
    # Log watcher to watch crs api for recieveing the ctask.
    echo("Watching crs api logs for incoming ctask..."),
    {
        name         => "wait-for-comp-server-to-task",
	    command      => [ "timeout", "8m", "../prt/logwatcher.pl",
			      "-f", sub { my $agent=shift; @files= glob $agent->testspec->results_dir . "/start-afc-lax-api-server.log"; return $files[0]; },
			      # Success! - stop if you see one of these
                     "-p", "API Task endpoint: task endpoint post_v1_task prompted.",
			      # ERRORs - stop if you see one of these
	                      # "-q", "ERROR", -- 20250425 - lbaez - for some reason there is an "ERROR: Delta scan failed" message
				# in the example-comp-server logs, when it actually did not fail ....
                    # Above is likely to be seen early if the example server is unable to clone aixcc repos
			      "-q", "An unhandled error occurred during initialization",
			      "-q", "An error occurred",
			      "-q", "Received signal number",
			      "-q", "Feature NOT licensed",
                              #   FIXME this should exit before afl
			      "-q", "ABORT",		# this is a thing AFL sez
			      "-q", "abort",
			      "-q", "; Exiting",
		],
  	    comparo      => undef,
	    complete_before_continuing => 1,
    },
    echo("Challenge Task received by our CRS API!")
    # Log watcher to watch the start-example-compeition-server logs for task triggering to succeed
    # {
    #     name         => "wait-for-comp-server-to-task",
	#     command      => [ "timeout", "4m", "../prt/logwatcher.pl",
	# 		      "-f", sub { my $agent=shift; @files= glob $agent->testspec->results_dir . "/start-example-competition-server.log"; return $files[0]; },
	# 		      # Success! - stop if you see one of these
    #                             # "-p", "http server started on",
    #                             "-p", "notified crs",
    #                             "-p", "Creating Delta Scan Task",
    #                             "-p", "Creating Full Scan Task",
    #                             "-p", sub { return ($full_scan == 1) ? "\"event_type\":\"new_full_scan\"" : "\"event_type\":\"new_delta_scan\"" ; },
	# 		      # ERRORs - stop if you see one of these
	#                       # "-q", "ERROR", -- 20250425 - lbaez - for some reason there is an "ERROR: Delta scan failed" message
	# 			# in the example-comp-server logs, when it actually did not fail ....
    #                 # Above is likely to be seen early if the example server is unable to clone aixcc repos
	# 		      "-q", "An unhandled error occurred during initialization",
	# 		      "-q", "An error occurred",
	# 		      "-q", "Received signal number",
	# 		      "-q", "Feature NOT licensed",
    #                           #   FIXME this should exit before afl
	# 		      "-q", "ABORT",		# this is a thing AFL sez
	# 		      "-q", "abort",
	# 		      "-q", "; Exiting",
	# 	],
  	#     comparo      => undef,
	#     complete_before_continuing => 1,
    # },
    # echo("Example competition is generating a task to send to our CRS!"),
);

our @trigger_sarif= (
    echo("Triggering competition server to send a sarif from the full scan to LAX CRS"),
    {
        name         => "trigger-sarif",
        command      => [
                            "../tools/trigger-sarif.sh",
                            sub { my $agent=shift; @files= glob $agent->testspec->results_dir . "/start-afc-lax-api-server.log"; return $files[0]; },
                            $sarif_script,
                        ],
        comparo      => undef,
	    exit_code    => undef,	# this b/c is daemon and will die badly at end of test
        complete_before_continuing =>0,
    },
    # Log watcher to watch the start-example-compeition-server logs for task triggering to succeed
    echo("Watching crs api logs for incoming sarif..."),
    {
        name         => "wait-for-sarif",
	    command      => [ "timeout", "8m", "../prt/logwatcher.pl",
			      "-f", sub { my $agent=shift; @files= glob $agent->testspec->results_dir . "/start-afc-lax-api-server.log"; return $files[0]; },
			      # Success! - stop if you see one of these
                     "-p", "API Sarif endpoint: sarif endpoint post_v1_sarif prompted.",
			      # ERRORs - stop if you see one of these
	                      # "-q", "ERROR", -- 20250425 - lbaez - for some reason there is an "ERROR: Delta scan failed" message
				# in the example-comp-server logs, when it actually did not fail ....
                    # Above is likely to be seen early if the example server is unable to clone aixcc repos
			      "-q", "An unhandled error occurred during initialization",
			      "-q", "An error occurred",
			      "-q", "Received signal number",
			      "-q", "Feature NOT licensed",
                              #   FIXME this should exit before afl
			      "-q", "ABORT",		# this is a thing AFL sez
			      "-q", "abort",
			      "-q", "; Exiting",
		],
  	    comparo      => undef,
	    complete_before_continuing => 1,
    },
    echo("Example competition is generating a sarif to send to our CRS!"),
);

our @start_example_competition_server = (
    echo("Starting example competition server"),
    {
        name         => "start-example-competition-server",
        command      => [ "../tools/example-competition-server/run-with-docker" ],
        comparo      => undef,
	    exit_code    => undef,	# this b/c is daemon and will die badly at end of test
        complete_before_continuing =>0,
    },
    {
        name         => "wait-for-example-competition-server",
	    command      => [ "timeout", "2m", "../prt/logwatcher.pl",
			      "-f", sub { my $agent=shift; @files= glob $agent->testspec->results_dir . "/start-example-competition-server.log"; return $files[0]; },
			      # Success! - stop if you see one of these
                                "-p", "http server started on",
			      # ERRORs - stop if you see one of these
			      "-q", "An unhandled error occurred during initialization",
			      "-q", "An error occurred",
			      "-q", "Received signal number",
			      "-q", "Feature NOT licensed",
                              #   FIXME this should exit before afl
			      "-q", "ABORT",		# this is a thing AFL sez
			      "-q", "abort",
			      "-q", "; Exiting",
		],
  	    comparo      => undef,
	    complete_before_continuing => 1,
    },
    echo("Example competition server has successfully started!"),
);

our @start_lax_crs_api = (
    echo("Starting Lacrosse CRS API"),
    {
        name         => "start-afc-lax-api-server",
        command      => [ "../../rsync/code/tools/afc-start-crs-api-server", sub { my $agent=shift; $dir=$agent->testspec->results_dir; return("$dir"); } ],
        comparo      => undef,
	exit_code    => undef,	# this b/c is daemon and will die badly at end of test
        complete_before_continuing =>0,
    },
    {
        name         => "wait-for-afc-lax-api-server",
	    command      => [ "timeout", "2m", "../prt/logwatcher.pl",
			      "-f", sub { my $agent=shift; @files= glob $agent->testspec->results_dir . "/start-afc-lax-api-server.log"; return $files[0]; },
			      # Success! - stop if you see one of these
                    	      "-p", "Uvicorn running",
			      # ERRORs - stop if you see one of these
			      "-q", "An unhandled error occurred during initialization",
			      "-q", "An error occurred",
			      "-q", "Received signal number",
			      "-q", "Feature NOT licensed",
                              #   FIXME this should exit before afl
			      "-q", "ABORT",		# this is a thing AFL sez
			      "-q", "abort",
			      "-q", "; Exiting",
		],
  	    comparo      => undef,
	    complete_before_continuing => 1,
    },
    echo("Lacrosse CRS API has successfully started!"),
);

our @start_agents = (
    @cluster_stats,
    @before_lisp_agents,
    echo("Starting Lacrosse agents"),
    $run_in_compose ?
        @compose_agents :
        {
            name         => "neofuzz-launcher",
            description  => "Really runs the Lacrosse agents",
            command      => [ nf_launcher(),
                                sub { my $agent=shift; return $agent->testspec->results_dir;},  # the test results dir
                                $experiment, "$numoptimi", "$numfbs", "$localhostonly", "$usegui", "$allocationmethod",
                              ],
            comparo      => undef,
            complete_before_continuing => 1,
        },

    #&delay(1),	# this is unfortunate but seems necessary b/c logwatcher gets undef filename if OPTIMUS0.log is not created fast enuf.
			# happily, the above launcher is running in parallel and always takes >1s, so this doesnt really hurt, just looks bad.

    ($usegui == 1) ? @start_gui : (),

    echo("Waiting for Lacrosse agents to all connect"),
    $numfbs ?
	{
	    name         => "wait_for_all_connected",
	    command      => [ "timeout", "10m", "../prt/logwatcher.pl",
			      "-f", sub { my $agent=shift; @files= glob $agent->testspec->results_dir . "/OPTIMUS0.log"; return $files[0]; },
			      # Success! - stop if you see one of these
                              "-p", "All expected AMPs connected",
			      # ERRORs - stop if you see one of these
			      "-q", "An unhandled error occurred during initialization",
			      "-q", "An error occurred",
			      "-q", "Received signal number",
			      "-q", "Feature NOT licensed",
			      "-q", "carry on...",
                              #   FIXME this should exit before afl
			      "-q", "ABORT",		# this is a thing AFL sez
			      "-q", "abort",
			      "-q", "; Exiting",
		],
  	    comparo      => undef,
	    complete_before_continuing => 1,
	} : (),
    echo("All Lacrosse agents connected"),

    ($run_siftfield && $ENV{'DISPLAY'}) ? @siftfield : (),

    ($run_with_api == 1) ? (
        @start_lax_crs_api
    ): (),
    ($run_with_local_comp == 1) ? (
        @start_example_competition_server,
        @trigger_example_competition_server_to_task,
    ): (),
    ($test_sarif == 1) ? (
        @trigger_sarif,
    ): (),

    @new_target_agents,

    ($success_pattern eq '') ? () : @success_agents,

    ($local_success_pattern eq '') ? () : @local_success_agents,

    ($use_optimus_comparo == 1) ? @optimus_comparo_agent : (),

    #get_container_logs(),

    ($run_shutdown == 1) ? @shutdown_agents : (),
    );

our @clean_and_start_agents = (
        sorted_printenv(),
	cp("../../../sandbox/env"),
	{
	    name => "print-results-dir",
	    description => "print the results dir to the prt stdout.",
	    command => ["true", sub { my $agent = shift; print "\nResults: " . $agent->testspec->results_dir . "\n"; } ],
	    comparo => undef,
	},
	{
	    name         => "git-version",
	    command      => [ "../tools/git-version" ],
  	    comparo      => undef,
	    exit_code    => undef,
	    complete_before_continuing => 1,
	},

        $run_dstat ? @dstat_agent : (),
	#@rsync_code, # run rsync-code before everything
     	@compile_lisp,
	@compile_mm_within_docker,
        #($compile_qemu_p == 1) ? @compile_qemu: (),
        ($compile_aflplusplus_hb_p == 1) ? @compile_aflplusplus_hb: (),
        @rsync_code, # FIXME this should really only run in non-compose mode or go away entirely
        # This is now being started by lax-run-optimus0 inside of the container
        #@ensure_mm,


		# the below will kill rsync jobs, so dont do it until rsync finishes.  Stray rsync jobs appear to cause ksoftirq process load problems.
	echo("Starting kill-stray-condor-processes"),
	{	## just in case last test didnt clean up well enuf, this goes to all condor nodes and nukes any remaining alisp/z3/fball jobs
	    name         => "kill-stray-condor-processes",
	    command      => [ "../tools/kill-stray-condor-processes" ],
  	    comparo      => undef,
	    exit_code    => undef,
	    complete_before_continuing => 1,
	},

	@start_agents,
  );

our @shutdown_agents =  (
	echo("Telling Optimus to shut it all down"),
	{
	    name         => "tello",
	    description  => "tells optimus to shut everyone down",
	    command      => [ "../tools/tell-optimus-halt" ],
  	    comparo      => undef,
	    exit_code    => undef,
	    complete_before_continuing => 1,
	},
	echo("Starting kill-stray-condor-processes"),
	{
	    name         => "shutdown-kill-stray-condor-processes",
	    command      => [ "../tools/kill-stray-condor-processes" ],
  	    comparo      => undef,
	    exit_code    => undef,
	    complete_before_continuing => 1,
	},
 );

 our @shutdown_local = (
    echo("Telling Optimus to shut it all down"),
    {
        name        => "tello",
        description => "tells optimus to shut everyone down",
        command     => [ "../matchmaker/tell-amp",
                "-n", "OPTIMUS0-AMP-$ENV{USER}-ACCEPTOR",
                "-k",
        ],
        comparo     => undef,
        complete_before_continuing  => 1,
    },
	echo("Starting kill-stray-local-processes"),
	{
	    name         => "shutdown-kill-stray-local-processes",
	    command      => [ "../tools/kill-stray-local-processes" ],
  	    comparo      => undef,
            exit_code    => undef,
	    complete_before_continuing => 1,
	},
	echo("Finished kill-stray-local-processes"),
);

sub killer {
  warn("Killing all test processes and user's docker containers\n");
  if ($run_in_compose) { system("../prt/cd-then-exec ../../../ /bin/make down"); }
  system("../tools/kill-stray-local-processes 1>/dev/null 2>/dev/null");
  # there is no such command
  # system("../tools/kill-demo-dockers 1>/dev/null 2>/dev/null");	# this gets the demo network too
  # FIXME: want to be able to find the logs...
  system("../tools/kill-my-dockers 1>/dev/null 2>/dev/null");
}

sub condor_killer {
  my $test=shift();
  #warn("Calling kill-stray-condor-processes");
  my $results_dir = $test->results_dir;

  if ($final_smoketest) {
      # cleanup after cp load prevention hack
      #system("ls -al ../../../cp_root/");
      system("rm ../../../cp_root/.pulled*");
      #system("rm -rf ../../../cp_root/*");
  }
  system("../tools/get-container-logs.sh ${results_dir}");

  system("../tools/kill-stray-condor-processes 1>/dev/null 2>/dev/null");
  #my $curdir=$ENV{PWD};
  #my $logdir= $agent->testspec->results_dir;
  #warn "logdir is $logdir\n";
  #chdir $logdir;
  #my $cluster=`../tools/cluster-number`;
  #my $curdir=`/bin/pwd`;
  #warn "curdir is $curdir\n";
  #system("../../release-cluster `cluster-number`

  #system("../tools/cd-then-exec $logdir ../release-cluster `cluster-number`");
  #warn("Calling release-my-clusers");
  system("../tools/release-my-clusters");

  killer();
  if ($run_in_compose) {
    system("../tools/cleanup-compose-agents");
  }
  if ($run_with_local_comp) {
      system("../tools/cleanup-after-comp-server");
  }
  #system("../tools/kill-demo-dockers");	# this gets the demo network too
  #system("../tools/kill-my-dockers");
}

# Sometimes you just just want to fail a test.
our @fail_agent = (
    {
        name => "fail_agent",
        description => "Always failing agent; poss useful for debugging.",
        command => 'false',
        exit_code => 0,
        comparo => undef,
    }
    );

our @dstat_agent = (
    {
        name         => "dstat",
        command      => [ "../prt/timestamp", "dstat", "-lcmsg", "--noupdate","--nocolor","--output",sub {my $agent=shift; return ($agent->testspec->results_dir . "/dstat-output.csv");},10 ],
        comparo      => undef,
        daemon =>1,
        must_survive => 0,
        exit_code    => undef,
        complete_before_continuing =>0,
    }
    );

# Like printenv in prt/common.pl but w env vars sorted alphabetically
sub sorted_printenv { 	# print all the env vars to a logfile for help debugging issues; note you must call this as printenv(), in the agent list, not just printenv,
    my @agent = ({
    	name => "printenv",
    	description => "print the env vars to a logfile for help debugging issues",
    	command   => ['perl', '-e', 'foreach my $var (sort keys(%ENV)) { print ("$var=$ENV{$var}\n");}; exit 0'],
        comparo => undef, },);
    return @agent;
}

sub cp_most_recent_scratch_to_results {
    my @agents = (
        echo("Copying goodstuff from crs_scratch to results."),
	{
	    name         => "cp-most-recent-scratch-to-results",
	    command      => [ "../tools/cp-most-recent-scratch-to.sh",
                              sub {my $agent=shift; return $agent->testspec->results_dir;}],
  	    comparo      => undef,
	    exit_code    => undef,
	    complete_before_continuing => 1,
	},
        );
    return @agents;
}

sub get_container_logs {
    my @agents = (
        echo("Putting container logs in results."),
	{
	    name         => "get-container-logs",
	    command      => [ "../tools/get-container-logs.sh",
                              sub {my $agent=shift; return $agent->testspec->results_dir;}],
  	    comparo      => undef,
	    exit_code    => undef,
	    complete_before_continuing => 1,
	},
        );
    return @agents;
}
