# ------------------------------------------------------------
# COPYRIGHT START
# Copyright (c) 2014, Smart Information Flow Technologies (SIFT).
# COPYRIGHT END
# ------------------------------------------------------------
# StrictLineComparo requires an exact match between the key_facts in
# the rubric and results. That is, unlike LineComparo, it does not
# allow for extra key_fact lines in the results.

# TODO If the "strict" flag is set to 0, this behaves exactly like
# LineComparo. When we figure out how to add this flag to LineComparo
# without disrupting its subclasses (i.e. without requiring all
# subclasses to *also* add the flag), we should move this
# compareToRubric() function into LineComparo with its strict flag set
# to 0.
package PRT::StrictLineComparo;

our @ISA = ("PRT::LineComparo");

use strict;
use Carp;
use FileHandle;

use PRT::Base;
use PRT::Config;

our $AUTOLOAD;  # it's a package global
our $CLASS = "PRT::StrictLineComparo";

# names and initializer of fields
my %_FIELDS = (
               # public:
               # semi-public (from test spec)
               show_comparisons   => 0,
               case_sensitive =>1,
               ignore_fact_regexp => undef,
               key_fact_regexp    => '.*',
               strict             => 1,
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

  if (PRT::Config::verboseMode()) {
    print "Configuration:\n";
    printf " - \$show_comparisons = %d\n", $self->show_comparisons;
    printf " - \$case_sensitive = %d\n", $self->case_sensitive;
    printf " - \$ignore_fact_regexp = %s\n", $self->ignore_fact_regexp;
    printf " - \$key_fact_regexp = %s\n", $self->key_fact_regexp;
  }

  my $thisRubricLineNum = 0;
  my $thisResultsLineNum = 0;
  my $lastRubricMatchLine = -1;
  my $lastResultsMatchLine = -1;
  my $lastRubricMatchString = "";
  my $lastResultsMatchString = "";

  # TODO Put the file reading and regex matching code into a function
  # which is reused for both rubric and results.
  if (!defined($self->key_fact_regexp)) {
    verbose("key_fact regexp is undef, ignoring entire rubric.\n");
  }
  else {
    # Wrap the whole expression so that we can refer to it if there
    # are no other groups specifed.
    my $fact_regexp = "(" . $self->key_fact_regexp . ")";

  RUBRIC_LINE:
    while (<$rubricFH>) {
      my $thisRubricLineString = $_;
      $thisRubricLineString =~ s/\r?\n$//; # remove CR so UNIX can compare to Windows
      ++$thisRubricLineNum;

      if ( ($self->case_sensitive && $thisRubricLineString =~ /$fact_regexp/) ||
           (!($self->case_sensitive) && $thisRubricLineString =~ /$fact_regexp/i) ) {
        # TODO Handle multiple groups. If no groups specified, use
        # entire string.
        my $theFact = $2;
        if (!defined($theFact)) {
          # Looks like no capturing group was present, use entire
          # match. Note that this and the results match need to do the
          # same thing.
          $theFact = $1;
        }
        print "   - Rubric line $thisRubricLineNum matches the key fact regex\n     $theFact\n"
          if $self->show_comparisons;
        my $ignore_fact_regexp = $self->ignore_fact_regexp;
        if (defined($ignore_fact_regexp)
            && ( ($self->case_sensitive && ($theFact =~ /$ignore_fact_regexp/)) ||
                 (!($self->case_sensitive) && ($theFact =~ /$ignore_fact_regexp/i)) ) ) {
          print "      - Fact matches ignore list, skipping\n" if $self->show_comparisons;
          next RUBRIC_LINE;
        }

        while (<$resultsFH>) {
          my $thisResultsLineString = $_;
          $thisResultsLineString =~ s/\r?\n$//; # remove CR so UNIX can compare to Windows
          ++$thisResultsLineNum;
          print "      - Comparing to results line $thisResultsLineNum,\n        $thisResultsLineString" if $self->show_comparisons>1;

          #if ($thisResultsLineString =~ /$fact_regexp/) {
          if ( ($self->case_sensitive && ($thisResultsLineString =~ /$fact_regexp/)) ||
               (!($self->case_sensitive) && ($thisResultsLineString =~ /$fact_regexp/i))) {
            my $results_fact = $2;
            if (!defined($results_fact)) {
              # Looks like no capturing group was present, use entire
              # match. Note that this and the rubric match need to do
              # the same thing.
              $results_fact = $1;
            }
            if ( ($self->case_sensitive && ($theFact eq $results_fact)) ||
                 ( !($self->case_sensitive) && (lc $theFact eq lc $results_fact)) ) {
              print "      * Matched results line $thisResultsLineNum,\n        $thisResultsLineString" if $self->show_comparisons==1;
              print "        Matched!\n" if $self->show_comparisons>1;
              $lastRubricMatchLine = $thisRubricLineNum;
              $lastResultsMatchLine = $thisResultsLineNum;
              $lastRubricMatchString = $thisRubricLineString;
              $lastResultsMatchString = $thisResultsLineString;
              next RUBRIC_LINE;
            } elsif ($self->strict) {
              print "FAILURE: fact in results line $thisResultsLineNum does not match fact in rubric line $thisRubricLineNum\n  $theFact\n  $results_fact\n.";
              $self->printLastMatch($lastRubricMatchLine, $lastResultsMatchLine,
                                    $lastRubricMatchString, $lastResultsMatchString);
              return 0;
            } else {
              print "        Not a match.\n" if $self->show_comparisons>1;
            }
          } else {
            print "        Not a fact post.\n" if $self->show_comparisons>1;
          }
        }

        print "FAILURE: could not find a match to rubric line $thisRubricLineNum:\n  $theFact\n.";
        $self->printLastMatch($lastRubricMatchLine, $lastResultsMatchLine,
                              $lastRubricMatchString, $lastResultsMatchString);
        return 0;
      } else {
        print "   - Rubric line $thisRubricLineNum is miscellaneous garbage.\n     $thisRubricLineString     Ignoring.\n"
          if $self->show_comparisons>1;
      }
    }
    # Rubric is exhausted. If strict, need to exhaust results as well.
    if ($self->strict) {
      while (<$resultsFH>) {
        my $thisResultsLineString = $_;
        $thisResultsLineString =~ s/\r?\n$//; # remove CR so UNIX can compare to Windows
        ++$thisResultsLineNum;
        if ($thisResultsLineString =~ /$fact_regexp/) {
          my $results_fact = $2 ? $2 : $1; # Take group if there, else is entire match.
          my $ignore_fact_regexp = $self->ignore_fact_regexp;
          if (defined($ignore_fact_regexp)
              && $results_fact =~ /$ignore_fact_regexp/) {
            print "   - Results line $thisResultsLineNum is miscellaneous garbage.\n     $thisResultsLineString     Ignoring.\n"
                if $self->show_comparisons>1;
          } else {
            print "FAILURE: could not find a match to results line $thisResultsLineNum:\n  $results_fact\n.";
            $self->printLastMatch($lastRubricMatchLine, $lastResultsMatchLine,
                                  $lastRubricMatchString, $lastResultsMatchString);
            return 0;
          }
        }
      }
    }
  }
  if ($self->strict) {
    print "  Exhausted rubric and results files; succeeding.\n" if $self->show_comparisons;
  } else {
    print "  Exhausted rubric file; succeeding.\n" if $self->show_comparisons;
  }
  return 1;
}

use PRT::Blessings::VerbatimCopyBlessing;
sub getBlessing {
  my $self = shift;
  return new PRT::Blessings::VerbatimCopyBlessing();
}

1;
