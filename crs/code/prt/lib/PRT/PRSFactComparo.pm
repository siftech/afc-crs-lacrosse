# ------------------------------------------------------------
# COPYRIGHT START
# Copyright (c) 2014, Smart Information Flow Technologies (SIFT).
# COPYRIGHT END
# ------------------------------------------------------------
# Comparo class for PRS facts
package PRT::PRSFactComparo;
use PRT::LineComparo;
our @ISA = ("PRT::LineComparo");

use strict;
use Carp;
use FileHandle;

use PRT::Base;

our $AUTOLOAD;  # it's a package global
our $CLASS = "PRT::PRSFactComparo";

# names and initializer of fields
my %_FIELDS = (
               # public:
               # semi-public (from test spec)
               case_sensitive => 0,     ## NOTE so doesnt matter if run PRS -l upper/lower
               show_comparisons   => 0,
               #ignore_fact_regexp => "USER-TRACE-START-TIME|START-TIME|<Op_instance 0x[A-Fa-f0-9]+>",
               ignore_fact_regexp => "user-trace-start-time|start-time|<Op_instance",
               key_fact_regexp    => "Posting the fact:.*",
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
