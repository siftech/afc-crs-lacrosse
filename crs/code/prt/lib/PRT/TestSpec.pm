# ------------------------------------------------------------
# COPYRIGHT START
# Copyright (c) 2014, Smart Information Flow Technologies (SIFT).
# COPYRIGHT END
# ------------------------------------------------------------
# PRT::TestSpec
# Everything related to a whole PRT Test, most importantly a bunch of PRT::TestAgents.
##

package PRT::TestSpec;
our $AUTOLOAD;  # it's a package global
our $CLASS = "PRT::TestSpec";
our @ISA = qw(PRT::Base);

use strict;

use Carp;
use Data::Dumper;
use File::Basename qw/ dirname basename fileparse /;
use File::Copy qw / copy /;
use File::Spec;

use PRT::Base;
use PRT::Config qw( :default :modes );
use PRT::Compare;

# names and initializer of fields
my %_FIELDS = (
               # public:
               name        => undef,
               description => "<no-description>",
               keywords    => [],
               includes    => [],
               agents      => [],
               agent_delay => 0,
               all_daemons_must_survive => 0,	# abort test early if this is on and any daemon fails
               # private:
               filename    => undef,
               results_dir => undef,
               rubric_dir => undef,
               lowercase_logfile_names => 1
              );
# Class-level return list of fields
sub FIELDS() { return \%_FIELDS; }

sub new {
  my $class = shift;

  # grab the (required) filename argument
  my $filename = {@_}->{filename};
  if (! $filename) {
    carp "no 'filename' argument specified to new on $class";
    return undef
  }

  # load the file (no longer using require, so can reload before actually run)
  my $result = do $filename;
  if (! $result) {
    if ($@) {
      carp "Unable to parse $filename\nError message:\n\t${@}\n";
    } elsif ($!) {
      carp "Unable to read $filename:\nError message:\n\t${!}\n;"
    }
    return undef;
  }

  # Defer to superclass constructor
  my $self = $class->SUPER::new(@_, %{$result});

  # ensure test has a test-name specified, or create one from filename
  unless(defined($self->{name})) {
    $self->{name} = fileparse($self->filename, qr/\.[^.]*$/);
  }

  # transform agents into a list of actual agent objects
  $self->agents( [ map { new PRT::TestAgent(%{$_}, testspec => $self) }
                       @{$self->agents} ] );

  # return the new object
  return $self;
}

sub rubric_dir {
  my $self = shift;
  croak "'$self' is not a '$CLASS'" unless ref($self) && $self->isa($CLASS);
  my $field = "rubric_dir";
  if (@_) {
    # set the value, and return it
    return $self->{$field} = shift;
  } elsif (! defined($self->{$field})) {
    # compute the rubric dir, from the filename and test name, if possible
    my $dir = $PRT::Config::RUBRIC_DEFAULT_DIR;
    $dir = Cwd::abs_path($dir);
    $dir = File::Spec->catdir($dir, $self->name);
    return $self->{$field} = $dir;
  }
  # return the value we have stored
  return $self->{$field};
}

sub toString {
  my $self = shift;
  croak "'$self' is not a '$CLASS'" unless ref($self) && $self->isa($CLASS);
  # Test (test filename)
  #   Description
  #   Agents:
  #    1. Name: Description
  #    2. ...

  if (! verboseMode()) {
    return sprintf("%s ('%s')\n",
                   $self->name, File::Spec->abs2rel( $self->filename ) );
  }

  # else, transform agents into a string for a more verbose output
  my $agents;
  {
    my $idx = 1;
    $agents = join("\n",
                   map { sprintf("     %d. %s", $idx++, $_->toString()) }
                   @{$self->agents} );
  }

  # transform rest, + agents, into a string and return it
  return sprintf(<<TOSTRING
Test: '%s' ('%s')
   Description: '%s'
   Results Dir: '%s'
   Rubrics Dir: '%s'
   Agents:
%s
TOSTRING
                    , $self->name, File::Spec->abs2rel( $self->filename ),
                    $self->description,
                    File::Spec->abs2rel( $self->results_dir ),
                     $self->rubric_dir,
                    #File::Spec->abs2rel( $self->rubric_dir ),
                    $agents);
}

sub copySpecToResultsDir {
  my $self = shift;
  croak "'$self' is not a '$CLASS'" unless ref($self) && $self->isa($CLASS);
  if (defined($self->{results_dir}) && -d $self->results_dir) {
    my $filecopy = File::Spec->catfile( $self->results_dir,
                                        basename( $self->filename ));
    debug("copying %s to %s\n", $self->filename, $filecopy);
    copy($self->filename, $filecopy)
      || carp sprintf("Copy of test specification file (%s) to results directory (%s) failed.", $self->filename, $self->results_dir);
  } else {
    carp "Cannot copy test specification to non-existtent directory: '$self->results_dir'";
  }

}

sub runTest {
  my $self = shift;
  croak "'$self' is not a '$CLASS'" unless ref($self) && $self->isa($CLASS);
#  my $test = shift;
#  # FIXME - add a timestamp to the test start
#  verbose("START running test: '%s'\n", $test->{testname});
#
#  verbose("END running test: '%s'\n", $test->{testname});
}



sub compareTestToRubric {
  my $self = shift;
  croak "'$self' is not a '$CLASS'" unless ref($self) && $self->isa($CLASS);

  my $rubric_dir = $self->rubric_dir;
#  if (! -d "$rubric_dir") {
#    warn sprintf("Test '%s' rubric dir '%s' does not exist.\n",
#                 $self->name, File::Spec->abs2rel($rubric_dir));
#    return $self->{_pass} = 0;
#  }

  my $results_dir = $self->results_dir;
  if (! -d "$results_dir") {
    warn sprintf("Test '%s' results dir '%s' does not exist.\n",
                 $self->name, File::Spec->abs2rel($results_dir));
    return $self->{_pass} = 0;
  }

  # get the current value of the _pass var in case someone set it to zero
  verbose("self_pass is [$self->{_pass}]\n");
  my $test_pass = defined($self->{_pass}) ? $self->{_pass} : 1;

  if (0==$test_pass) { verbose("Test is declared failed before comparing rubrics\n"); }

  foreach my $agent (@{$self->agents}) {
    if (!defined($agent->comparo)) {
      verbose("Test '%s' agent '%s' comparo is null; ignoring\n",
              $self->name, $agent->name);
      next;
    }

    my $agent_log = $agent->logfile;
    my $rubric_log = File::Spec->catfile($rubric_dir,
                                         $agent->rubric_file_rootname);
    if (! -e "$rubric_log") {
      warn sprintf("Test '%s' agent '%s' rubric does not exist; expected at '%s'.\n",
                   $self->name, $agent->name, File::Spec->abs2rel($rubric_log));
      $test_pass = 0;
      next;
    }
    if (! -e "$agent_log") {
      warn sprintf("Test '%s' agent '%s' result log does not exist, the agent probably did not execute; expected at '%s'.\n",
                   $self->name, $agent->name, File::Spec->abs2rel($agent_log));
      $test_pass = 0;
      next;
    }

    ## If the comparo will do all-lines consideration (such as for
    ## performance assessment), we allow the comparo to set itself
    ## here --- deeper in the call to compareFiles, we won't have
    ## access to the agent and TestSpec.
    $agent->comparo->initializeLinesConsumer($agent, $self)
        if $agent->comparo->isa("PRT::AllLinesConsumer");

    debug("comparing\n  rubric: '%s'\n  log: '%s'\n", $rubric_log, $agent_log);
    my $passed_rubric = PRT::Compare->compareFiles($agent_log, $rubric_log, $agent->comparo);
    if (defined($agent->{_pass})) {
      $agent->{_pass} &&= $passed_rubric;
    } else {
      $agent->{_pass} = $passed_rubric;
    }
    $test_pass &&= $agent->{_pass};

    if ($agent->comparo->isa("PRT::AllLinesConsumer")) {
      my @allLinesResult = $agent->comparo->finalizeLinesConsumer();
      my $allLinesPass = shift @allLinesResult;
      debug(" - checking all-lines result: ",
            ($allLinesPass ? "passed" : "failed"), "\n");
      $agent->{_pass} &&= $allLinesPass;
      $test_pass &&= $allLinesPass;
      if (($#allLinesResult == 0 && $allLinesResult[0] ne '')
          || ($#allLinesResult > 0)){
        print "WARNING: " if $allLinesPass;
        printf @allLinesResult;
        print "\n";
      }
    }
  }

  return $self->{_pass} = $test_pass;
}

sub printResults {
  my $self = shift;
  croak "'$self' is not a '$CLASS'" unless ref($self) && $self->isa($CLASS);

  verbose("\nResults of test '%s': ", $self->name);
  my $testpass = $self->{_pass};
  #printf("%s (%s)", ($testpass ? "pass" : "FAIL"), $self->name);
  printf("%s", ($testpass ? "pass" : "FAIL"));
  verbose(" (rubric '%s')", File::Spec->abs2rel($self->rubric_dir));
  printf("\n");
  if (!$testpass || verboseMode()) {
    my $idx = 1;
    foreach my $agent (@{$self->agents}) {
      printf(" - Agent %d '%s' (exit: %d): %s\n",
             $idx++, $agent->name, $agent->result,
             (! defined($agent->{_pass}) ? "NOT COMPARED" :
              ($agent->{_pass} ? "pass" : "FAIL")));
    }
  }
  verbose("\n\n");

  return $self->{_pass} ? 1 : 0;
}

sub blessTestResults {
}

sub findTestResults {
  my $self = shift;
  croak "'$self' is not a '$CLASS'" unless ref($self) && $self->isa($CLASS);



  #return @dirs;
}

## eof PRT::TestSpec.pm
