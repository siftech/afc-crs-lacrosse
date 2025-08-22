=pod

=head1 NAME

PRT::PerfChecker - Mixin providing property-values accumulation and
checking.

=head1 SYNOPSIS

  use PRT::GenericLineComparo;
  use PRT::PerfChecker;
  use parent qw(PRT::GenericLineComparo PRT::PerfChecker);

=head1 DESCRIPTION

This package defined a mixin class for comparos which check
performance from regular expressions in the logs holding
stats. ("Performance" being something of a euphamism for whatever the
labeled values we collect represent.)

This class implements the methods defined in L<PRT::AllLinesConsumer>.
Instructions for the contents of the C<perfcheck> hash in test agent
spec hashes are given in the main PRT documentation.

=cut

# ------------------------------------------------------------
# COPYRIGHT START
# Copyright (c) 2014, Smart Information Flow Technologies (SIFT).
# COPYRIGHT END
# ------------------------------------------------------------

package PRT::PerfChecker;
use strict;
our @ISA = ("PRT::AllLinesConsumer");
use PRT::Perf;
use PRT::Config;
use Data::Dumper;

our $defaultDefaultLabels = ['.*', '.*'];
our $nextTestId=0;

## Sets up internal structures for performance data collection, and
## for fleshing out the perfcheck hash with defaults.
sub initializeLinesConsumer {
  my $self = shift;
  my $agent = shift;
  my $testSpec = shift;

  ## A particular test may or may not have a perfchecks field set, and
  ## we allow it to live in many places for creative defaulting.  So
  ## we look for it.
  my $rawPerfChecks = findSlotDefinition('perfchecks',
                                         $self, $agent, $testSpec);

  ## If we found a perfchecks array, expand any syntactic sugar, and
  ## store a copy locally.
  if (defined $rawPerfChecks) {

    ## Some defaults that we may find in the enclosures.
    my $defaultCompFn = findSlotDefinition('defaultCompFn',
                                           $self, $agent, $testSpec);
    my $defaultCombineFn = findSlotDefinition('defaultCombineFn',
                                              $self, $agent, $testSpec);
    my $defaultLabels = findSlotDefinition('defaultPerfTestLabels',
                                           $self, $agent, $testSpec);
    $defaultLabels = $defaultDefaultLabels unless defined $defaultLabels;
    $defaultCombineFn = PRT::Perf::overwrite_combiner
        unless defined $defaultCombineFn;

    my @checks = ();

    ## Look at each check in the array in turn.
    foreach my $perfCheck (@$rawPerfChecks) {

      ## If the check is given as a hash table, make sure it has the
      ## necessary slot values.
      if (ref $perfCheck eq 'HASH') {
        ## There's no default for the regex --- if it's not provided,
        ## we just bomb out.
        die "No regex slot given for perfcheck"
            unless exists $perfCheck->{regex};

        ## Look at each test on labels hooked to this regex, and make
        ## sure we have a comparison function for the values stored
        ## against those labels.  We also assign each labeller an
        ## internal ID --- does it make sense if a result and rubric
        ## property are accumulated by different rules? Can we even
        ## test to see if those rules have the same comparison
        ## function (i.e. does Perl let us do pointer equality on
        ## thunks)?
        if (exists $perfCheck->{tests}) {
          foreach my $test (@{$perfCheck->{tests}}) {
            $test->{labels} = $defaultDefaultLabels
                unless exists $test->{labels};

            if (!(exists $test->{compFn})) {
              if (exists $perfCheck->{defaultCompFn}) {
                $test->{compFn} = $perfCheck->{defaultCompFn};
              } elsif (defined $defaultCompFn) {
                $test->{compFn} = $defaultCompFn;
              } else {
                die ("No default and no compFn for $perfCheck->{regex} labels "
                     . join('::', @{$test->{labels}}));
              }
            }

            if (!(exists $test->{combineFn})) {
              $test->{combineFn} = $defaultCombineFn;
            }

            $test->{id} = ++$nextTestId;

            print("Test #", $test->{id}, " labels: ",
                  join(", ",@{$test->{labels}}), "\n")
                if PRT::Config::debugMode()>2;
          }
        }

      } else {
        die "No default compFn provided for bare perfcheck $perfCheck"
            unless defined $defaultCompFn;
        $perfCheck = { regex => $perfCheck, compFn => $defaultCompFn,
                       value => -1 };
      }

      push @checks, $perfCheck;
    }

    $self->{perfRubric} = [];
    $self->{perfResult} = [];
    $self->{perfChecksActual} = \@checks;
  }

  ## print "perfChecks ", Dumper($self->{perfChecksActual})
  ##     if PRT::Config::debugMode()>2;
  return;
}

sub findSlotDefinition {
  my $slotName = shift;
  while ($#_ >= 0) {
    my $hash = shift;
    return $hash->{$slotName} if exists $hash->{$slotName};
  }
  return undef;
}

## Called when all lines are consumed, and a pass/fail judgment should
## be made.
sub finalizeLinesConsumer {
  my $self = shift;
  my $rubricPerf = $self->{perfRubric};
  my $resultPerf = $self->{perfResult};
  my $boolResult=1;
  my $textOut=undef;
  my $textSep='';
  print "Checking accumulated stat lines\n" if PRT::Config::debugMode()>1;

  ## Print out the arrays of hashes of collected values if we're
  ## debugging hard enough.
  print "perfRubric ", Dumper($rubricPerf) if PRT::Config::debugMode()>2;
  print "perfResult ", Dumper($resultPerf) if PRT::Config::debugMode()>2;

  ## Walk through the rubric hashes, and make sure the corresponding
  ## result hash entries measure up.
  my @rubricBundles = sort { bundleSort($a,$b) } $self->getLabelBundles($rubricPerf);
  foreach my $rubricBundle (@rubricBundles) {
    my $label = $rubricBundle->{label};
    my $val   = $rubricBundle->{val};
    my $id    = $rubricBundle->{id};
    print "- for ", join(":",@$label), " rubric has $val"
        if PRT::Config::debugMode()>1;

    my $resultBundle = pullBundle($label, $resultPerf);
    if (defined $resultBundle) {
      print ", result has ", $resultBundle->{val}, "\n"
          if PRT::Config::debugMode()>1;
      my $compResult = &{$rubricBundle->{compFn}}($rubricBundle->{val},
                                                  $resultBundle->{val});
      print "  - comparison gives $compResult\n" if PRT::Config::debugMode()>2;
      if ($compResult) {
        print "  - passed\n" if PRT::Config::debugMode()>1;
      } else {
        $boolResult=0;
        $textOut = '' unless defined $textOut;
        my $msg = $PRT::Perf::msg;
        $msg='(failed without detail)' unless defined $msg;
        print "  - Failed: $msg\n" if PRT::Config::debugMode()>1;
        $textOut .= $textSep . "For '" . join("'/'", @{$resultBundle->{label}})
            . "': " . $msg;
        $textSep = "\n";
      }
    } else {
      print ", result not defined\n" if PRT::Config::debugMode()>1;
      $boolResult=0;
      $textOut = '' unless defined $textOut;
      $textOut .= $textSep . "No results for label "
          . join(':', @{$rubricBundle->{label}});
      $textSep = "\n";
    }

  }
  return ($boolResult, $textOut);
}

sub pullBundle {
  my $label = shift;
  my $hasharray = shift;
  my $labelLen = 1+$#{$label};

  if (exists $hasharray->[$labelLen]) {
    my $scanner = $hasharray->[$labelLen];
    print "      - length $labelLen\n" if PRT::Config::debugMode()>2;

    foreach my $l (@$label) {
      if (exists $scanner->{$l}) {
        print "      - hop $l\n" if PRT::Config::debugMode()>2;
        $scanner = $scanner->{$l};
      } else {
        print "      - no hop for $l\n" if PRT::Config::debugMode()>2;
        return undef;
      }
    }

    print "      - result ", Dumper($scanner) if PRT::Config::debugMode()>2;
    return $scanner;
  } else {
    print "      - no length $labelLen bundles\n" if PRT::Config::debugMode()>2;
    return undef;
  }
}

sub bundleSort {
  my $la = $a->{label};
  my $lb = $b->{label};
  my $ncomp = length($la) <=> length($lb);
  return $ncomp if $ncomp;
  for(my $i=0; $i<=$#{$la}; ++$i) {
    my $scomp = $la->[$i] cmp $lb->[$i];
    return $scomp if $scomp;
  }
  return 0;
}

sub getLabelBundles {
  my $self = shift;
  my $rubricPerf = shift;
  my @result=();

  for(my $arity=0; $arity<=$#{$rubricPerf}; ++$arity) {
    my $hash = $rubricPerf->[$arity];
    getBundlesAt(\@result, $arity, $hash) if defined $hash;
  }

  return @result;
}

sub getBundlesAt {
  my $acc=shift;
  my $arity=shift;
  my $hash=shift;

  if ($arity<1) {
    if (defined $hash && exists $hash->{label}) {
      push @$acc, $hash;
    }
  } else {
    foreach my $k (keys %$hash) {
      getBundlesAt($acc, $arity-1, $hash->{$k});
    }
  }

  return;
}

## Consume a line of the rubric. No result returned.
sub consumeRubricLine {
  my $self = shift;
  my $rubricLine = shift;
  print "PerfCheck sees rubric line: $rubricLine"
      if PRT::Config::debugMode()>2;
  $self->storeProperty($rubricLine, $self->{perfRubric});
  return;
}

## Consume a line of the results log. No result returned.
sub consumeResultsLine {
  my $self = shift;
  my $resultLine = shift;
  print "PerfCheck sees results line: $resultLine"
      if PRT::Config::debugMode()>2;
  $self->storeProperty($resultLine, $self->{perfResult});
  return;
}

## Method shared by consumeRubricLine and consumeResultsLine for
## squirreling away the labeled values.
sub storeProperty {
  my $self = shift;
  my $string = shift;
  my $sorter = shift;

  foreach my $check (@{$self->{perfChecksActual}}) {
    my $regex = $check->{regex};
    if ($string =~ /$regex/) {
      print " - Matches $regex\n" if PRT::Config::debugMode()>2;
      my @capture=();
      for(my $i=1; $i<=$#+; ++$i) {
        my $captured = substr $string, $-[$i], ($+[$i] - $-[$i]);
        print "   capture $i [$-[$i],$+[$i]): $captured\n"
            if PRT::Config::debugMode()>2;
        $capture[$i] = $captured;
      }

      ## First get the captured expression corresponding to the value.
      ## If the given index is negative, that's from the end of the
      ## capture array, so with maximum capture group index N the
      ## actual index is (N+GIVEN+1).
      my $valueIdx = $check->{value};
      $valueIdx = -1 unless defined $valueIdx;
      $valueIdx = $#capture+$valueIdx+1  if $valueIdx<0;

      ## Next get the indices into capture groups which form the
      ## label.  By default, it's all of them except the value's
      ## index.
      my $labelIdxs = $check->{labelIndices};
      if (!(defined $labelIdxs)) {
        $labelIdxs = [];
        for(my $i=1; $i<=$#capture; ++$i) {
          push @$labelIdxs, $i unless $i == $valueIdx;
        }
      }
      print("   - Label indices: ", join(',', @$labelIdxs), "\n")
            if PRT::Config::debugMode()>2;

      ## Run through the tests to find the ones which apply.
    TEST_APPLIES:
      foreach my $test (@{$check->{tests}}) {
        my $labelRegexps = $test->{labels};
        my $compFn       = $test->{compFn};
        my $combineFn    = $test->{combineFn};
        my $testId       = $test->{id};
        my $labelRegexpsLen = 1+$#{$labelRegexps};
        print("   - Considering for test ", join('/', @$labelRegexps), "\n")
            if PRT::Config::debugMode()>2;

        ## "Apply" means that the regular expressions given in the
        ## test's labels slot match the substrings captured from the
        ## given input line.  Firstly, if there aren't enough captured
        ## substrings, then it doesn't apply (this is actually a bug
        ## in the test spec).
        if ($#capture < $labelRegexpsLen) {
          if (PRT::Config::debugMode()>2) {
            print "     Found $#{$labelIdxs} labels, needed $labelRegexpsLen\n";
          }
          next TEST_APPLIES;
        }

        ## And if there are enough, then we make sure they match the
        ## regular expressions.
        for(my $labelIdx=0; $labelIdx<$labelRegexpsLen; ++$labelIdx) {
          my $labelRe = $labelRegexps->[$labelIdx];
          my $captured = $capture[$labelIdxs->[$labelIdx]];
          if (!($captured =~ /$labelRe/)) {
            print("     Label $labelIdx \"", $captured,
                  "\" does not match RE \"", $labelRe, "\"\n")
                if PRT::Config::debugMode()>2;
            next TEST_APPLIES;
          } else {
            print("     Label $labelIdx \"", $captured,
                  "\" matches RE \"", $labelRe, "\"\n")
                if PRT::Config::debugMode()>2;
          }
        }

        # If we get to here then the subset of labels match.  Hooray!
        # Now the challenge is to file away the property values.
        if ($labelRegexpsLen == 0) {
          # Degenerate case of no labels.  Whatever.  Not quite that
          # easy to crash us.
          print "     Filing under [$labelRegexpsLen]=", $capture[$valueIdx], "\n"
              if PRT::Config::debugMode()>2;
          $sorter->[$labelRegexpsLen] = { val => $capture[$valueIdx],
                                          compFn => $compFn,
                                          id => $testId,
                                          label => [] };
        } else {
          # So then in general it's hash tables all the way down.  For
          # example, if there are three labels L1, L2 and L3, then we
          # want
          #
          # $sorter->[3]{L1}{L2}{L3} = $capture[$valueIdx]
          #
          # But of course this is done dynamically.  We set up
          # $finder, initially set to just the $sorter->[3], and then
          # update via loop
          #
          # $finder = $finder->{L1};
          # $finder = $finder->{L2};
          #
          # for all but the last label.  With the last label we make
          # the assignment $finder{L3} = $capture[$valueIdx]; .
          print "     Filing under [$labelRegexpsLen]"
              if PRT::Config::debugMode()>2;
          $sorter->[$labelRegexpsLen] = { }
              unless exists $sorter->[$labelRegexpsLen];
          my $finder = $sorter->[$labelRegexpsLen];
          my @labelVector = ();

          for(my $labelIdx=0; $labelIdx<$labelRegexpsLen-1; ++$labelIdx) {
            my $label = $capture[$labelIdxs->[$labelIdx]];
            push @labelVector, $label;
            print "{$label}" if PRT::Config::debugMode()>2;
            $finder->{$label} = {} unless exists $finder->{$label};
            $finder = $finder->{$label};
          }

          my $lastLabel = $capture[$labelIdxs->[$labelRegexpsLen-1]];
          push @labelVector, $lastLabel;
          my $theValue = $capture[$valueIdx];

          ## Check whether we already have a value stored for the
          ## property --- combine them if so, and install the fresh
          ## value otherwise.
          if (exists $finder->{$lastLabel}) {
            warn "Property assigned by different rules"
                if $finder->{$lastLabel}{id} != $testId;
            my $combined = &$combineFn($finder->{$lastLabel}{val}, $theValue);
            print("{", $lastLabel, "} updated to ", $combined, "\n")
                if PRT::Config::debugMode()>2;
            $finder->{$lastLabel}{val} = $combined;
          } else {
            print("{", $lastLabel, "}=", $theValue, "\n")
                if PRT::Config::debugMode()>2;
            $finder->{$lastLabel} = { val => $theValue,
                                      compFn => $compFn,
                                      id => $testId,
                                      label => \@labelVector };
          }
        }
      }
    } else {
      print " - Does not match $regex\n" if PRT::Config::debugMode()>2;
    }
  }
}

1;
