# ------------------------------------------------------------
# COPYRIGHT START
# Copyright (c) 2014, Smart Information Flow Technologies (SIFT).
# COPYRIGHT END
# ------------------------------------------------------------
# Comparo class for Prism simulation analysis
package PRT::PrismSimulationComparo;
use PRT::LineComparo;
our @ISA = ("PRT::LineComparo");

use strict;
use Carp;
use FileHandle;

use PRT::Base;

our $AUTOLOAD;  # it's a package global
our $CLASS = "PRT::PrismSimulationComparo";

# names and initializer of fields
my %_FIELDS = (
               # public:
               # semi-public (from test spec)
               show_comparisons   => 0,
               case_sensitive =>1,
               ignore_fact_regexp => undef,
               key_fact_regexp    => "((^Simulating:|^Simulation (method|method parameters|parameters|result details):|^Testing result:).*)",
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
