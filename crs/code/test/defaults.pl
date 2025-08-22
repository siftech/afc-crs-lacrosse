# Should be used at top of every test to reset default values, since the perl will carry them along from prior tests (sorry, icky).
# In earlier versions we were having files only set default values if there wasnt already a defined value, but that pattern is
# flawed because tests run in the same perl as the prior test.

our $home=$ENV{'NEO_FUZZ_HOME'};
if ($home eq '') { $home = `(cd ../..; /bin/pwd)`; chomp $home;}	# assume we are running in code/test
# note that for any non-localhost run, must use $home/rsync/code...and probably always anyways

our $use_ccl_in_docker = 1;	# for now, ONLY supporting CCL-in-docker operations
our $use_sbcl = 0; # this will override use of CCL.
our $use_ccl = not $use_sbcl;

sub sbcl_test () {
  $use_ccl_in_docker = 1;
  $use_ccl = 0;
  $use_sbcl = 1;
}

sub ccl_test () {
  $use_ccl_in_docker = 1;
  $use_ccl = 1;
  $use_sbcl = 0;
}

$host=`hostname`; chomp $host; #print $host; 	# NOTE add new cluster submit hosts here

our $cp_config = "../crs-sandbox-items/sift/mock-cp-config.yaml";

our $run_in_compose=0;
our $run_dstat = 0;
our $numoptimi = 1;
our $numfbs = 0;		# defaults to single agent, optimus, for now... he figures out he has to work, I guess
our $numdvms = 0;	# is this still needed?
our $localhostonly = 0;	# set to 1 to launch only one OPTIMUS0 and no FBs, just on the localhost you are running prt on
our $usegui = 0; # set to 1 if you wamt to use the gui
our $success_timeout = "70000m";
our $success_pattern = '';
our $local_success_pattern = '';
our $success_logfile = "OPTIMUS0.log";
our $failure_pattern = "this is an unlikely fail pattern";
our $run_shutdown = 1; 	# should we get the auto shutdown after Optimus prints success_pattern
# We can compare the OPTIMUS0.log against a rubric if desired.
our $use_optimus_comparo = 0;
our $optimus_comparo_logfile = "OPTIMUS0.log";
our $optimus_comparo_regexp = "RESULT:.*";
our $optimus_comparo = new PRT::LineComparo(
                           #show_comparisons   => 2,
                           #ignore_fact_regexp => undef,
                           key_fact_regexp    => $optimus_comparo_regexp);
our $optimus_perfchecks = [];

our $sleeptime = "0";	# how long to sleep after find success pattern and before logwatcher exits; this can be useful for isabel analysis after last crash

our $net = "";  # Can be used to supply a complete network argument to docker run to specify how the container

# try running siftfield if you have it in path; it's not in NF codebase, for now
our $run_siftfield = system("which sift-field.py 1>/dev/null 2>/dev/null") ? 0 : 1;

our $siftfield_isa_daemon = 1;   # if 1, dont wait for SF to finish to end the test, kill it when everything else is done except daemons.
# This is what you want to have tests use SF if possible but not wait at end for user to kill SF; for demo prt files, turn this off (to zero).
# Having this at zero is handy so that you can see the SF display after the test is over, capture it, etc, then kill the SF window and the test wraps up.
# Note on Jenkins the SF always dies right away b/c of no display, which is fine, as long as you
# dont turn on both this flag and all_daemons_must_survive :-)

our $driller_must_survive = 1;

if ($ENV{'DOCKER_SUBNET'} eq "") {
    $ENV{'DOCKER_SUBNET'} = "172.18.9";
}

our $POV_SERVER_IP = $ENV{'POV_SERVER_IP'};
if ($POV_SERVER_IP eq '') { $POV_SERVER_IP = '127.0.0.1'};  #warn "POV_SERVER_IP defaulting to $POV_SERVER_IP\n";
our $POV_SERVER_PORT = $ENV{'POV_SERVER_PORT'};
if ($POV_SERVER_PORT eq '') { $POV_SERVER_PORT = $ENV{'CIRCA_BASEPORT'}};  #warn "POV_SERVER_PORT defaulting to $POV_SERVER_PORT\n";

our $analystio_client_logfile = "OPTIMUS0.log";
our $compile_whatclib_p = 0;  # switches build in tests that use clean_and_start_agents
our $compile_qemu_p = 0;      # switches build in tests that use clean_and_start_agents
our $compile_aflplusplus_hb_p = 0;      # switches build in tests that use clean_and_start_agents

# Any agents in this list run before the lisp agents even start
our @before_lisp_agents = ();

# Any agents in this list run after all agents connect and before success_agents
our @new_target_agents = ();

our $hh_expected_vulns = 53;

our $hh_analysis_file = "analysis-trace.yaml";
# ##################################

# Lacrosse defaults
our $lacrosse_utils_must_survive = 1;

$ENV{'LAX_LOAD_CP_IMAGES'} = "true";
$final_smoketest = 0;
our $run_with_local_comp = 0;
our $run_with_api = 0;
our $task_script = "libpng-delta.ts";
our $full_scan = 0;
our $test_sarif=0;

1;	# leave this here at the very end, so 'do'ing this file succeeds
