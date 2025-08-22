# ------------------------------------------------------------
# COPYRIGHT START
# Copyright (c) 2014, Smart Information Flow Technologies (SIFT).
# COPYRIGHT END
# ------------------------------------------------------------
package PRT::Blessings::Blessing;
our @ISA = ("PRT::Base");
use strict;
use Carp;
use PRT::Base;

# Class-level return list of fields
sub FIELDS() { return {}; }

sub new {
  my $class = shift;
  # Defer to superclass constructor
  my $self = $class->SUPER::new(@_);
  return $self;
}

sub bless_rubric {
  my $self = shift;
  my $dest = shift;
  my $results_log_filename = shift;
  my $agent_spec = shift;
  my $test_spec = shift;
  return;
}
