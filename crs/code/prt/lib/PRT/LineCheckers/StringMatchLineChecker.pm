# ------------------------------------------------------------
# COPYRIGHT START
# Copyright (c) 2014, Smart Information Flow Technologies (SIFT).
# COPYRIGHT END
# ------------------------------------------------------------
package PRT::LineCheckers::StringMatchLineChecker;
our @ISA = ("PRT::Base");
use strict;
use Carp;
use FileHandle;
use PRT::Base;

# names and initializer of fields
my %_FIELDS = (
  # public:
  # semi-public:
  show_comparisons => 0,
  # private:
  target => undef,
    );
# Class-level return list of fields
sub FIELDS() { return \%_FIELDS; }

sub new {
  my $class = shift;
  my $target = shift;
  # Defer to superclass constructor
  my $self = $class->SUPER::new(@_);
  $self->target($target);
  # return the new object
  return $self;
}

sub check {
  my $self = shift;
  my $string = shift;
  my $target = $self->target();
  my $result = $string eq $target;
  print "     ** Comparing string \"$string\"
        to target \"$target\":
        result: $result\n"
        if $self->show_comparisons();
  return $result;
}

sub blurb {
  my $self = shift;
  return "(eq \"" . $self->target() . "\")";
}
