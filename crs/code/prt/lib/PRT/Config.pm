# ------------------------------------------------------------
# COPYRIGHT START
# Copyright (c) 2014, Smart Information Flow Technologies (SIFT).
# COPYRIGHT END
# ------------------------------------------------------------
# PRT::Config
# Holds PRT config and common stuff
##

package PRT::Config;
our $AUTOLOAD;  # it's a package global
our $CLASS = "PRT::Config";

use strict;

require Exporter;
our @ISA = qw(Exporter);
# Breaking all the rules and exporting subroutines!
our @EXPORT      = qw(debug verbose verbose2 warning deprecated);
our @EXPORT_OK   = qw(debugMode verboseMode dryrunMode demoMode);
our %EXPORT_TAGS = (default => [@EXPORT], modes => [@EXPORT_OK]);

sub debug(@)      { printf(@_) if $PRT::Config::DEBUG; }
sub verbose(@)    { printf(@_) if $PRT::Config::DEBUG || ($PRT::Config::VERBOSE >= 1); }
sub verbose2(@)   { printf(@_) if $PRT::Config::DEBUG || ($PRT::Config::VERBOSE >= 2); }
sub deprecated(@) { warning("DEPRECATED: " . $_[0], @_[1..$#_]) if ($PRT::Config::WARN); }
sub warning(@)    { printf("WARNING: " . $_[0], @_[1..$#_]) if ($PRT::Config::WARN); }

sub debugMode()   { return $PRT::Config::DEBUG; }
sub verboseMode() { return ($PRT::Config::VERBOSE >=1) || debugMode() || dryrunMode() }
sub dryrunMode()  { return $PRT::Config::DRYRUN; }
sub demoMode()    { return $PRT::Config::DEMO; }

# global/implicitly "exported", variables, thus use 'our'
# http://stackoverflow.com/questions/845060/what-is-the-difference-between-my-and-our-in-perl
our $DRYRUN = 0;
our $VERBOSE = 0;
our $WARN = 1;
our $DEBUG = 0;
our $DEMO = 0;
our $TEST_DEFAULT_DIR = ".";
our $RUBRIC_DEFAULT_DIR = File::Spec->catdir(Cwd::cwd(), "rubrics");
our $RESULTS_DEFAULT_DIR = File::Spec->catdir(Cwd::cwd(), "results");

# PRS defaults

our $OPRS_DEFAULT    = "oprs";
our @OPRS_ARGS       = qw / -l upper -a -e /;
# Currenty, these are passed on the command line, but I'll bet we
# could open a server connection and end with a "run"
our @OPRS_COMMANDS   = (
                        "trace user trace on",
                        "trace user fact on",
                        #"trace user goal on",
                        "declare predicate user-trace-start-time",
                        "add (user-trace-start-time (current (time)))",
                        #"show version",
                        "add (achieve (main))",
                       );
our @OPRS_COMMANDS_DEMO = (
                           "trace user trace off",
                           "trace all off",
                           "add (achieve (main))",
                          );

# eof PRT::Config.pm
