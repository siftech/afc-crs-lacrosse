# ------------------------------------------------------------
# COPYRIGHT START
# Copyright (c) 2014, Smart Information Flow Technologies (SIFT).
# COPYRIGHT END
# ------------------------------------------------------------
# PRT::LispFiveamComparo
#
# This file contains a single, customizable comparison class that
# checks output for Lisp compile warnings/errors and Fiveam test suite
# failures.
#
# Unfortunately, the fiveam tests do not report errors as they
# go. Instead, we need to wait until the end to see if the tests pass
# or not. Moreover, the error details do not have a nice format that
# we can match with a regex. Instead, we just need to look for the
# failure count and ensure that it is 0.
#
# FIXME An advance solution might find the summary area and allow any
# failures that were present in the rubric. Really, though we don't
# want our tests to ever include failures.
#
# LIMITATIONS:
#
# The compile errors/warnings are extracted from the rubric and stored
# in a list. When we encounter a warning in the results, we look for
# the warning in the list. This ignores the fact that the same warning
# text might be printed multiple times because an equivalent error
# occurs in multiple files or even multiple functions within a single
# file. I took a stab at fixing this and discovered that line breaks
# cause problems with this. Ultimately, allowing warnings to exist at
# all is undesirable.
#
# IMPLEMENTATION:
#
# This is derived from the PRT::Base to deal with opening the
# file-handles. We only use these when looking for compile warnings
# though. And, when looking for compile warnings, the contents of the
# results file control execution, not the lines in the rubric -- the
# rubric is only used to determine what warnings are allowable.

package PRT::LispFiveamComparo;

our @ISA = ("PRT::Base");

use strict;
use Carp;
use FileHandle;

use PRT::Base;

use PRT::Config qw( :default verboseMode );

our $AUTOLOAD;  # it's a package global
our $CLASS = "PRT::LispFiveamComparo";

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
               # default to existing behavior of not checking the test count
               check_test_count => 0,

               # semi-public (from test spec)
               show_comparisons   => 0,
               # Compile warnings that match this will be ignored.
               ignore_fact_regexp => undef,
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

  # The resultsFH and rubricFH are each of type FileHandle: they
  # should be open; they will be read only; and the file must exist.
  my $resultsFH = shift;
  my $rubricFH  = shift;

  if (verboseMode()) {
    print "Configuration:\n";
    printf " - \$fail_on_warnings = %d\n", $self->fail_on_warnings;
    printf " - \$show_comparisons = %d\n", $self->show_comparisons;
    printf " - \$ignore_fact_regexp = %s\n", $self->ignore_fact_regexp;
    # printf " - \$key_fact_regexp = %s\n", $self->key_fact_regexp;
  }

  # First, find all of the warnings and the test count in the rubric.
  my @rubric_warnings = ();
  my $test_count = 0;
  while (my $rubric_line = <$rubricFH>) {

    if ($rubric_line =~ /Did (\d+) checks?./) {
      if ($self->show_comparisons > 1) {
        print("Found test count in rubric: $rubric_line");
      }

      if ( $test_count == 0 ) {
        $test_count = $1;
      } else {
        die "Found two sets of FiveAM tests in the rubric: can't handle this.";
      }
    }

    if ($rubric_line =~ /^Warning:/) {
      if ($self->show_comparisons > 1) {
        print("Found warning in rubric: $rubric_line");
      }
      push(@rubric_warnings, $rubric_line);
    }
  }

  # Now, check the results file.
  my $total_failure_count = 0;
  my $tests_performed = -1;
  my $warning_count = 0;
  my $excused_by_rubric = 0;
  my $ignore_matcher = $self->ignore_fact_regexp;

  while(my $line = <$resultsFH>) {

    # Look for the failure report.
    if($line =~ /\s+Fail:\s+(\d+)/) {
      my $fail_count = $1;
      if(verboseMode()) {
        print("Found error report with $fail_count failures.\n");
      }
      $total_failure_count += $fail_count;
    }

    # Look for the test count
    if ($line =~ /Did (\d+) checks?./) {
      if ( $tests_performed == -1 ) {
        $tests_performed = $1;
        if(verboseMode()) {
          print("Found test count: $tests_performed.\n");
        }
      } else {
        die "Found more than one set of FiveAM tests in the results.  Can't handle this.";
      }
    }


    # Also, look for any compile warnings.
    if ($line =~ /^Warning:/) {
      if ($self->show_comparisons > 0) {
        print "Found warning: $line";
      }

      if ( ($line =~ /Warning: Redefining .* in deflookup-table named/) ||
           ($ignore_matcher && ($line =~ /$ignore_matcher/)) ) {
        if ($self->show_comparisons > 0) {
          print("Ignoring warning that matches ignore regexp.\n");
        }
      }
      else {
        ++$warning_count;

        # Now we need to try to find a warning in the rubric...
        # If found, increment $excused_by_rubric;
        my $found_warning_in_rubric = 0;
        foreach my $rubric_warning (@rubric_warnings) {
          if ($line eq $rubric_warning) {
            ++$found_warning_in_rubric;
          }
        }
        if (0 < $found_warning_in_rubric) {
          if ($self->show_comparisons > 0) {
            print("Found warning in rubric, excusing.\n");
          }
          ++$excused_by_rubric;
        }
        else {
          print("Found unexcused warning: $line");
        }
      }
    }
  }

  if ( $tests_performed == -1 ) {
        print "Did not find any count of tests performed.\n";
        return 0;
  }

  if(verboseMode()) {
    print("Found $warning_count warnings, $excused_by_rubric are excused by rubric.\n");
    print("Found $total_failure_count total test failures.\n");
    if($self->check_test_count) {
      print("Performed $tests_performed tests, rubric expects $test_count.\n");
    } elsif ( $tests_performed < $test_count ) {
        print "Warning: Performed $tests_performed tests which is less than the $test_count that rubric expects.\n";
      }
  }



  if($total_failure_count > 0) {
    # We had test failures.
    print "Failing because we found $total_failure_count failures.\n";
    return 0;
  } elsif(($self->fail_on_warnings == 1) and
          ($warning_count > $excused_by_rubric)) {
    # We had unexcused compile warnings.
    print "Failing due to unexpected compiler warnings.\n";
    return 0;
  } elsif(($self->fail_on_warnings == 2) and
          ($warning_count > 0)) {
    # We had compile warnings, and they are disallowed.
    print "Failing due to compiler warnings; none are allowed.\n";
    return 0;
  } elsif($self->check_test_count and
          ($tests_performed != $test_count)) {
    print "Failing because the performed test count of $tests_performed is not equal to the rubric's $test_count test count.\n";
    return 0;
  } else {
    # No problems!
    return 1;
  }
}

use PRT::Blessings::VerbatimCopyBlessing;
sub getBlessing {
  my $self = shift;
  return new PRT::Blessings::VerbatimCopyBlessing();
}

1;
