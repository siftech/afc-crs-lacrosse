# ------------------------------------------------------------
# COPYRIGHT START
# Copyright (c) 2014, Smart Information Flow Technologies (SIFT).
# COPYRIGHT END
# ------------------------------------------------------------
# PRT::TestAgent
# Each agent in a PRT::TestSpec is represneted by one of these.
##

package PRT::TestAgent;
our $AUTOLOAD;  # it's a package global
our $CLASS = "PRT::TestAgent";
our @ISA = qw(PRT::Base);

use strict;

use Carp;
use IPC::Run qw//; # do not import any symbols

use PRT::Config qw( :default :modes );
use PRT::Blessings::Blessing;
use PRT::PRSFactComparo;        # leave this in for convenience for now, not really needed
use PRT::LineComparo;

# names and initializer of fields
my %_FIELDS = (
               # public:
               name        => undef,
               description => undef,
               includes    => [],
               command     => undef,
               executable  => undef,    # Deprecated
               add_args    => [],       # Deprecated
               # semi-public (from test spec)
               exit_code   => 0,
               daemon      => 0,
               must_survive => 0,		# only useful if daemon; abort test early if daemon fails and this is on
               complete_before_continuing => 0,
               comparo     => new PRT::LineComparo(),
               # results
               result      => undef,
               # private:
               testspec    => undef,
               run_handle  => undef,
               logfile     => undef,
               remote_user => undef,
               remote_host => undef,
               remote_log  => 0
              );
# Class-level return list of fields
sub FIELDS() { return \%_FIELDS; }

sub new {
  my $class = shift;
  # Defer to superclass constructor
  my $self = $class->SUPER::new(@_);

  # ensure test has a test-name specified
  unless(defined($self->{name})) {
    carp sprintf("Agent specification missing required 'name' field; file: '%s'",
                 $self->testspec->filename);
    return undef;
  }
  # return the new object
  return $self;
}

sub command {
  my $self = shift;
  croak "'$self' is not a '$CLASS'" unless ref($self) && $self->isa($CLASS);

  if (@_) {
    # Set the given value, but don't just return it
    $self->{command} = shift;
    # now fall through to process it 'normally' (recursively)
  }

  # Check to see if set
  if (! exists($self->{command}) || ! defined($self->{command})) {

    # Check deprecated form
    if (defined($self->{executable})) {
      # handle deprecated mode using 'executable' and 'add_args'
      deprecated("computing command from 'executable' and 'add_args' for agent '%s'.\n", $self->name);
      my @deprecated = ( $self->{executable} );
      push @deprecated, @{$self->{add_args}}
        if defined($self->{add_args});
      #print join " ", @deprecated, "\n";
      return $self->command(\@deprecated);

    } else {
      # No deprecated form exists, and undefined - return undef
      carp sprintf("non-existant or undef 'command' for agent: '%s'",
                   $self->name);
      return undef;
    }
  }

  # We have something now, at least we think so.
  my $command = $self->{command};
  #print "'$command'\n";

  if (defined($self->{remote_host}) && defined($self->{remote_user})) {
    splice @{$self->{command}}, 0, 0, 'ssh', '-t', '-t', "$self->{remote_user}\@$self->{remote_host}";
  }

  if ($self->{remote_log} eq '1') {
    if (defined($self->{logfile})) {
      push @{$self->{command}}, "> ~/remoteLogs/$self->{logfile}";
    } else {
      push @{$self->{command}}, "> ~/remoteLogs/$self->{name}.log";
    }
  }

  my $executable = $self->{executable}; # deprecated old form
  my $add_args = $self->{add_args};     # deprecated old form
  if (defined($command) && ref($command) eq '' && ref(\$command) eq 'SCALAR') {
    #warn "\n!!!!!!found SCALAR";
    return [ $command ];
  } elsif (ref($command) eq 'ARRAY') {
    #carp "\n!!!!!found ARRAY";
    # walk the array, expanding any code references, (but we don't recurse).
    my @result = ();
    foreach my $elem (@{$command}) {
      push @result, ( (ref($elem) eq 'CODE')
                      ? &$elem($self)  # eval and exapnd thunk.
                      : $elem );       # assume just normal/scalar
    }
    #print join " ", @result, "\n";
    return \@result;
  } elsif (ref($command) eq 'CODE') {
    #carp "\n!!!!!found CODE";
    my @result = &$command($self);
    return \@result;
  }

  warning("Don't know how to decode command of type '%s' for agent: '%s'",
          ref($command), $self->name);
  return qw ( false );
}

sub logfile {
  my $self = shift;
  croak "'$self' is not a '$CLASS'" unless ref($self) && $self->isa($CLASS);
  my $field = "logfile";

  if (@_) {
    # Set the given value, and return it.
    return $self->{$field} = shift;

  } elsif (! defined($self->{$field})) {
    # Set to the default value, and return it.
    my $logfile;
    if($self->testspec->lowercase_logfile_names == 1) {
      $logfile = File::Spec->catfile($self->testspec->results_dir,
                                      sprintf('%s.log', lc $self->name));
    } else {
      $logfile = File::Spec->catfile($self->testspec->results_dir,
                                      sprintf('%s.log', $self->name));
    } 
    return $self->{$field} = $logfile;

  } elsif (defined($self->{$field}) && !($self->{$field} =~ m%^(/|\.\.?/)%)) {
    # If we have a defined logfile, and if it is NOT an absolute
    # pathname (i.e. starts with /) or explicitly a relative pathname
    # (i.e. starts with ./ or ../), then prepend the results directory
    # to it.
    return File::Spec->catfile($self->testspec->results_dir, $self->{$field});
  }

  # Return the absolute, or explicitly relative, path we have stored
  return $self->{$field};
}

sub rubric_file_rootname {
  my $self = shift;
  croak "'$self' is not a '$CLASS'" unless ref($self) && $self->isa($CLASS);

  my $logfile = $self->logfile;
  if (defined $logfile) {
    if ($logfile =~ m|/(([^/])+)$|) {
      return $1;
    } else {
      return $logfile;
    }
  } else {
    ## If things are so bad that logfile returns undef, then it's bad
    ## enough for us to do the same.
    return undef;
  }
}

sub toString {
  my $self = shift;
  croak "'$self' is not a '$CLASS'" unless ref($self) && $self->isa($CLASS);
  return sprintf("'%s' - %s", $self->name, $self->description);
}

sub start {
  my $self = shift;
  croak "'$self' is not a '$CLASS'" unless ref($self) && $self->isa($CLASS);

  my $command = $self->command;
  my $logfile = $self->logfile;

  if (debugMode() || dryrunMode()) {
    printf("DEBUG: '%s'\n  Command: '%s'\n  Output: '%s'\n",
           $self->name,
           join(" ", map { m/\s/ ? "'$_'" : $_} @{$command}),
           $logfile);
  }
#
#  my $rawcmd = (split /\s+/, $command[0];
#
#  if (! (-x $rawcmd)){
#  	die("No execute permissions set on %s\n", $rawcmd);
#	}
#
#  if (&& (! (-r $rawcmd)) (-T $rawcmd)) {
#  	die("No read permissions set on text script %s\n", $rawcmd);
#	}

  unless (dryrunMode()) {
    my $ah = IPC::Run::start($command, '<', \undef, '>&', $logfile);#, timeout(215));
    $self->run_handle($ah);
    return $ah;
  }

  return undef;
}

sub finish {
  my $self = shift;
  croak "'$self' is not a '$CLASS'" unless ref($self) && $self->isa($CLASS);

  my $handle =  $self->run_handle;
  if (! defined $handle) {
    carp sprintf("%s has no run handle; perhaps never started?", $self->name);
    return 0;
  } else {
    $handle->finish();
    if (defined($self->{remote_user}) && defined($self->{remote_host}) && $self->{remote_log} eq '1') {
      system("scp", "-p", "$self->{remote_user}\@$self->{remote_host}:~/remoteLogs/$self->{name}.log", "$self->{logfile}");
    }
    return $handle->result();
  }
}

sub kill {
  my $self = shift;
  croak "'$self' is not a '$CLASS'" unless ref($self) && $self->isa($CLASS);

  my $handle =  $self->run_handle;
  if (! defined $handle) {
    carp sprintf("%s has no run handle; perhaps never started?", $self->name);
    return 0;
  } else {
    # Use IPC::Run's ability to kill the subjobs first
    $handle->kill_kill();
    # then make sure by sending term to my entire process group
    #kill TERM => -$$;
    if (defined($self->{remote_user}) && defined($self->{remote_host}) && $self->{remote_log} eq '1') {
      system("scp", "-p", "$self->{remote_user}\@$self->{remote_host}:~/remoteLogs/$self->{name}.log", "$self->{logfile}");
    }
    return $handle->result();
  }
}

sub check_exit_code {
  my $self = shift;
  croak "'$self' is not a '$CLASS'" unless ref($self) && $self->isa($CLASS);
  my $actual_exit_code = shift;

  my $ident = $self->name;
  my $abort_test = 0;
  my $expected_exit_code = $self->exit_code;
  if (defined($expected_exit_code)) {
    debug("** Expect exit code $expected_exit_code for agent $ident\n");
    $abort_test = ($actual_exit_code != $expected_exit_code);
  } else {
    debug("** Ignoring exit code for agent $ident\n");
  }

  if ($abort_test) {
    print STDERR "\n** Agent '$ident' exited with code $actual_exit_code, but test expected $expected_exit_code\n";
    if ($actual_exit_code == 124) {
    	print STDERR "** NOTE: Exit code 124 is usually a timeout!\n";
	}
    $self->{_pass} = 0;
    $self->testspec->{_pass} = 0;
  }
  return $abort_test
}

sub getBlessing {
  my $self = shift;
  return $self->{blessing}  if exists $self->{blessing};
  my $comparo = $self->comparo();
  if (defined $comparo) {
    my $comparoBlessing = $comparo->getBlessing();
    return $comparoBlessing  if defined $comparoBlessing;
  }
  return new PRT::Blessings::Blessing();
}

## eof PRT::TestAgent.pm
