# ------------------------------------------------------------
# COPYRIGHT START
# Copyright (c) 2014, Smart Information Flow Technologies (SIFT).
# COPYRIGHT END
# ------------------------------------------------------------
# PRT::LispNstComparo
#
# Very simple comparo, using no rubric, that looks for the final NST
# results summary in a file and makes sure no tests fail. By default,
# it will also check that none warn.
package PRT::LispNstComparo;

our @ISA = ("PRT::Base");

use strict;
use Carp;
use FileHandle;

use PRT::Base;

use PRT::Config qw( :default verboseMode );

our $AUTOLOAD;  # it's a package global
our $CLASS = "PRT::LispNstComparo";

# names and initializer of fields
my %_FIELDS = (
               # public:

               # 0 - ignore compile warnings.
               # 1 - fail on compile warnings that aren't in rubrics
               #     (i.e. new compile warnings).
               # 2 - fail on any compile warnings.
               #
               # Default: 1
               fail_on_warnings   => 1,

               # semi-public (from test spec)
               show_comparisons   => 1,
 # Compile warnings that match this will be ignored.
 # ignore_fact_regexp => undef,
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

sub compareToRubric() {
  my $self = shift;
  croak "'$self' is not a '$CLASS'" unless ref($self) && $self->isa($CLASS);

  # The resultsFH must be open FileHandles; it will be read only; and
  # the file must exist. The rubricFH is ignored.
  my $resultsFH = shift;
  my $rubricFH  = shift;

  if (verboseMode()) {
    print "Configuration:\n";
    printf " - \$fail_on_warnings = %d\n", $self->fail_on_warnings;
    printf " - \$show_comparisons = %d\n", $self->show_comparisons;
    # printf " - \$ignore_fact_regexp = %s\n", $self->ignore_fact_regexp;
    # printf " - \$key_fact_regexp = %s\n", $self->key_fact_regexp;
  }

  my $totalPassed;
  my $totalFound;
  my $totalFailed;
  my $totalErrors;
  my $totalWarnings;
  while(my $line = <$resultsFH>) {
    chomp;
    if ($line =~ /^TOTAL: ([0-9]+) of ([0-9]+) passed \(([0-9]+) failed, ([0-9]+) errors?, ([0-9]+) warnings?\)/) {
    # if ($line =~ /^TOTAL: ([0-9]+) of ([0-9]+) passed /) {
      print "Found \"$line\"\n"  if verboseMode();
      $totalPassed   = $1;
      $totalFound    = $2;
      $totalFailed   = $3;
      $totalErrors   = $4;
      $totalWarnings = $5;
    } else {
      print "Skipping \"$line\"\n"  if verboseMode();
    }
  }

  # There's no results summary in the results file.
  if (!(defined $totalFound)) {
    print "There's no results summary in the results file." if verboseMode();
    return 0;
  }

  unless ($totalErrors == 0) {
    print "There are $totalErrors error(s)." if verboseMode();
    return 0;
  }
  unless ($totalPassed == $totalFound) {
    print "totalPassed $totalPassed < totalFound $totalFound." if verboseMode();
    return 0;
  }
  return 0 unless $totalWarnings == 0 || !fail_on_warnings();
  return 1;
}

use PRT::Blessings::VerbatimCopyBlessing;
sub getBlessing {
  my $self = shift;
  return new PRT::Blessings::Blessing();
}

1;
