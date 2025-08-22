# ------------------------------------------------------------
# COPYRIGHT START
# Copyright (c) 2014, Smart Information Flow Technologies (SIFT).
# COPYRIGHT END
# ------------------------------------------------------------
# Comparo class for PRS goals
package PRT::PRSGoalComparo;
use PRT::LineComparo;
our @ISA = ("PRT::LineComparo");

use strict;
use Carp;
use FileHandle;

use PRT::Base;

our $AUTOLOAD;  # it's a package global
our $CLASS = "PRT::PRSGoalComparo";

# names and initializer of fields
my %_FIELDS = (
               # public:
               # semi-public (from test spec)
               case_sensitive =>0,
               show_comparisons   => 0,
               # For now, all goal expressions are ignored.
               ignore_fact_regexp => ".*",
               key_fact_regexp    => "Posting the goal:.*",
               # private:
              );
# Class-level return list of fields
sub FIELDS() { return \%_FIELDS; }

sub new {
  my $class = shift;
  # Defer to superclass constructor
  my $self = $class->SUPER::new(@_);

  # return the new object
  return $self;
}

sub getBlessing {
  my $self = shift;
  return new PRT::Blessings::VerbatimCopyBlessing();
}

1;
