# ------------------------------------------------------------
# COPYRIGHT START
# Copyright (c) 2014, Smart Information Flow Technologies (SIFT).
# COPYRIGHT END
# ------------------------------------------------------------
# TrdbgLineComparo

package PRT::TrdbgLineComparo;

our @ISA = ("PRT::GenericLineComparo");

use strict;
use Carp;
use FileHandle;

use PRT::GenericLineComparo;

our $AUTOLOAD;  # it's a package global
our $CLASS = "PRT::TrdbgLineComparo";

# names and initializer of fields
my %_FIELDS = (
               # public:
               # semi-public (from test spec)
               show_comparisons => 0,
               # private:
              );
# Class-level return list of fields
sub FIELDS() { return \%_FIELDS; }

# Not sure why this would need to repeated for every subclass --- will
# it not be inherited?
#
# sub new {
#   my $class = shift;
#   # Defer to superclass constructor
#   my $self = $class->SUPER::new(@_);
#
#   # return the new object
#   return $self;
# }

sub useRubricLine {
  my $self = shift;
  my $line = shift;
  return $line !~ /^\s*;/;
}

sub commonLinePreformat {
  my $self = shift;
  my $line = shift;
  my $result = $self->SUPER::commonLinePreformat($line);
  $result =~ s/^(\.\s+)+//; # remove CR so UNIX can compare to Windows
  return $result;
}

1;
