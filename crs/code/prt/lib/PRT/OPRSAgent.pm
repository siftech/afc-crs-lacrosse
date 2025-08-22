# ------------------------------------------------------------
# COPYRIGHT START
# Copyright (c) 2014, Smart Information Flow Technologies (SIFT).
# COPYRIGHT END
# ------------------------------------------------------------
# PRT::OPRSAgent
# Agent with special code for [X]OPRS
##

package PRT::OPRSAgent;
our $AUTOLOAD;  # it's a package global
our $CLASS = "PRT::OPRSAgent";
our @ISA = qw(PRT::TestAgent);

use strict;

use Carp;
use Data::Dumper;
use IPC::Run qw//; # do not import any symbols

use PRT::Config qw( :default :modes );

# names and initializer of fields
my %_FIELDS = (
               # public:
               name        => undef,
               description => undef,
               includes    => [],
               executable  => $PRT::Config::OPRS_DEFAULT,
               add_args    => [],
               # semi-public: (used by PRT::TestSpec)
               exit_code   => 0,
               daemon      => 0,
               complete_before_continuing => 0,
               comparo     => new PRT::PRSFactComparo(),
               # results
               result      => undef,
               # private:
               testspec    => undef,
               run_handle  => undef,
               logfile     => undef,
              );
# Class-level return list of fields
sub FIELDS() { return \%_FIELDS; }

sub start {
  my $self = shift;
  croak "'$self' is not a '$CLASS'" unless ref($self) && $self->isa($CLASS);

  my $executable = $self->executable;
  my $logfile = $self->logfile;
  my @args;

  if (ref($executable) eq 'CODE') {
    my @out = &$executable($self, $self->{testspec});
    $executable = shift @out;
    @args = @out;

    if (debugMode() || dryrunMode()) {
      print "DEBUG:  Calculated executable \"$executable\"";
      if ($#args>-1) {
        print " and arguments\n";
        my $argnum=0;
        foreach my $arg (@args) {
          print "        ", ++$argnum, ". $arg\n";
        }
      } else {
        print " and no arguments\n";
      }
    }
  }

  # Handle (x)oprs-specific configuration
  if ($executable =~ /x?oprs$/) {
      @args = ( @PRT::Config::OPRS_ARGS );
      # NOTE: because we are passing the args in via an array rather than
      # a string, make sure _NOT_ to quote the arguments that happen to
      # have spaces in them.
      push @args, "-n", $self->name;
      push @args, map { ( "-x", $_ ) } @{$self->includes};
      push @args, map { ( "-x", $_ ) } @{$self->testspec->includes};
      if ($PRT::Config::DEMO) {
        push @args, map { ( "-c", $_ ) } @PRT::Config::OPRS_COMMANDS_DEMO;
      } else {
        push @args, map { ( "-c", $_ ) } @PRT::Config::OPRS_COMMANDS;
      }
      if ($executable =~ /xoprs$/) {
          # xoprs has a special log option
          push @args, "-log", "$logfile";
          $logfile = "$logfile.stderr";
      }
  }

  # add additional args specified on a per-agent basis
  push @args, @{$self->add_args};

  if (debugMode() || dryrunMode()) {
    printf("DEBUG: '%s'\n  Executable: '%s'\n  Args: '%s'\n  Output: '%s'\n  Command:\n     %s\n",
           $self->name,
           $executable,
           join("', '", @args),
           $logfile,
           join(" ", ($executable, (map { m/\s/ ? "'$_'" : $_} @args))));
  }

  unless (dryrunMode()) {
    my $ah = IPC::Run::start([$executable, @args], '<', \undef, '>&', $logfile);#, timeout(215));
    $self->run_handle($ah);
    return $ah;
  }

  return undef;
}

## eof PRT::OPRSAgent.pm
