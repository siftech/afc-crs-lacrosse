# ------------------------------------------------------------
# COPYRIGHT START
# Copyright (c) 2014, Smart Information Flow Technologies (SIFT).
# COPYRIGHT END
# ------------------------------------------------------------
# PRT::Base
# Contains the base PRT::Base class which provides our core class functionality.
# Is the parent class for other PRT classes (usually)
##

package PRT::Base;
our $AUTOLOAD;  # it's a package global
our $CLASS = "PRT::Base";

use strict;

use Carp;
use Data::Dumper;

# Generic constructor
sub new {
  my $class = shift;
  #print "in $class->new()\n";
  my $fields = $class->FIELDS();
  my $self = {
              _permitted => $fields,
              @_,
             };
  bless $self, $class;
  return $self;
}

#my %_FIELDS = ();
#sub FIELDS() { return \%_FIELDS; }

sub DESTROY {
  # could remove this object from the appropriate hashes
}

# Provide all accessors transparently based upon the "_permitted" fields, with defaults
sub AUTOLOAD {
  my $self = shift;
  my $type = ref($self);
  # not helpful in this generic context; wait until we know more to give useful error
  #croak "'$self' is not a '$CLASS'" unless $type && $self->isa($CLASS);
  my $name = $AUTOLOAD;
  croak "$name not defined as function or permitted field."
    unless $type && $self->isa($CLASS);

  # see if "permitted"/direct field
  $name =~ s/.*://;   # strip fully-qualified portion
  # special case for mixed case function conventions
  if (! ($name eq "getBlessing")) {
      $name = lc $name;   # make lower case
  }
  if (exists $self->{_permitted}->{$name} ) {
    if (@_) {
      # assumes single value - should it allow arrays?
      return $self->{$name} = shift;
    } else {
      # If the value of the lookup exists, we return it, otherwise we
      # return the default value (stored as the value in the hash of
      # permitted keys).  Use `exists` over `defined` to allow undef
      # values in the object.
      return (exists($self->{$name}) ? $self->{$name} : $self->{_permitted}->{$name});
      # return (defined($self->{$name}) ? $self->{$name} : $self->{_permitted}->{$name});
      # return $self->{$name};
    }
  }

  croak "Cannot access `$name' field in class $type";
}

## eof PRT::Base.pm
