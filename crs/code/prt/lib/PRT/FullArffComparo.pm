# ------------------------------------------------------------
# COPYRIGHT START
# Copyright (c) 2014, Smart Information Flow Technologies (SIFT).
# COPYRIGHT END
# ------------------------------------------------------------
# PRT::FullArffComparo
#
# This file contains a single, customizable comparison class which
# provides line-by-line comparison of output logs which are ARFF
# files.  Ultimately, we want may want to add additional comparison
# classes, most notably one which looks for the existence or absence
# of specific lines in the output.

# Comparo class for ARFF file contents
package PRT::FullArffComparo;

our @ISA = ("PRT::LineComparo");

use strict;
use Carp;
use FileHandle;

use PRT::Base;

our $AUTOLOAD;  # it's a package global
our $CLASS = "PRT::FullArffComparo";

our $singleColumnRegEx = "(-?[0-9]+(\\.[0-9]+([eE]-?[0-9]+)?)?|true|false)";
our $commaSepColumnsRegEx = "($singleColumnRegEx(,$singleColumnRegEx)*)";
our $attributeRegEx = "(\@attribute\\s+.+\\s+.+)";
our $dataRegEx = "\@data\\s*\$";

# names and initializer of fields
my %_FIELDS = (
               # public:
               # semi-public (from test spec)
               case_sensitive =>1,
               show_comparisons   => 0,
               ignore_fact_regexp => undef,
               key_fact_regexp    => $commaSepColumnsRegEx,
               # private:
              );
# Class-level return list of fields
sub FIELDS() { return \%_FIELDS; }

# Not sure why this would need to repeated for every subclass --- will
# it not be inherited?

sub new {
  my $class = shift;
  # Defer to superclass constructor
  my $self = $class->SUPER::new(@_);

  # return the new object
  return $self;
}

use PRT::Blessings::VerbatimCopyBlessing;
sub getBlessing {
  my $self = shift;
  return new PRT::Blessings::VerbatimCopyBlessing();
}

1;
