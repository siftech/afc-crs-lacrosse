# ------------------------------------------------------------
# COPYRIGHT START
# Copyright (c) 2014, Smart Information Flow Technologies (SIFT).
# COPYRIGHT END
# ------------------------------------------------------------
# PRT::Compare - $Id$
#
# This file contains a single, customizable comparison class which
# provides line-by-line comparison of output logs. Ultimately, we want
# may want to add additional comparison classes, most notably one
# which looks for the existence or absence of specific lines in the
# output.
#
# Here is a list of planned changes to the line-by-line comparison.
#
#  - Match all groups from the key_fact pattern, if there are multiples.
#  - Add flags
#    - Allow/disallow extra facts in the results.
#  - Consider making show_comparisons a runtime flag (maybe verbose ==
#    3?)
#
# Here is what has been done:
#
# - Create a test for the comparo.
# - Make the PRSFactComparo and PRSGoalComparo subclasses which initialize
#   the parameters appropriately.
# - Make the pointer-skip pattern part of the ignore fact regexp.
#
# - Move the "Posting a fact" and "Posting a goal" strings to the
#   key_fact regexes in the subclasses.
# - Rework the comparison code to get the fact directly from the key_fact
#   regex. This simplifies the comparison a little bit.
# - If the key_fact regex is undef, skip the comparison entirely.
# - If the ignore regex is undef, nothing is ignored.
# - Add tests with the key_fact and ignore set to undef.
#
# - If no capturing group is specified by a key_fact, use the entire
#   matched expression.

package PRT::Compare;

use strict;
use Carp;
use FileHandle;

use PRT::Config; # qw( :default verboseMode );
use PRT::Blessings::VerbatimCopyBlessing;

sub compareFiles() {
  my $package     = shift @_;
  my $resultsFile = shift @_;
  my $rubricFile  = shift @_;
  my $comparo     = shift @_;

  my $rubricFH = FileHandle->new($rubricFile, "r")
    || croak "Opening '$rubricFile' failed.";
  my $resultsFH = FileHandle->new($resultsFile, "r")
    || croak "Opening '$resultsFile' failed.";

  if ($comparo->show_comparisons) {
    print "Comparing results file $resultsFile to rubric $rubricFile\n";
  }

  ## FIXME Eric, is the comma and carriage return a (presumably
  ## spurious) typo?
  my $result = $comparo->compareToRubric($resultsFH, $rubricFH), "\n";

  debug("\nResult from core comparo: %d\n", $result);
  $rubricFH->close();
  $resultsFH->close();
  return $result;
}

1;
