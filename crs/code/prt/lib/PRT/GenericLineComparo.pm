# ------------------------------------------------------------
# COPYRIGHT START
# Copyright (c) 2014, Smart Information Flow Technologies (SIFT).
# COPYRIGHT END
# ------------------------------------------------------------
# Comparo class for general purpose line-by-line checking of outputs.
package PRT::GenericLineComparo;
our @ISA = ("PRT::Base");
use strict;
use Carp;
use FileHandle;
use PRT::Base;
use PRT::Config qw( :default verboseMode );
use PRT::LineCheckers::StringMatchLineChecker;
use PRT::LineCheckers::StringMatchLineCheckerWithMemory;
use PRT::Blessings::VerbatimCopyBlessing;
our $AUTOLOAD;  # it's a package global
our $CLASS = "PRT::GenericLineComparo";

# names and initializer of fields
my %_FIELDS = (
  # public:
  # semi-public
  show_comparisons => 0, # (from test spec)
  # private:
    );
# Class-level return list of fields
sub FIELDS() { return \%_FIELDS; }

sub new {
  my $class = shift;
  # Defer to superclass constructor
  my $self = $class->SUPER::new(@_);
  $self->{enforceRubricLine} = 1;
  $self->{patternMatch} = 0;
  $self->{patternStartDelimRegex} = '\?{{';
  $self->{patternEndDelimRegex} = '}}';
  $self->{patternNameRegex} = '[-_a-zA-Z0-9]+';
  $self->{matchPattern} = '.*';

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
    printf " - \$show_comparisons = %d\n", $self->show_comparisons;
    #printf " - \$ignore_fact_regexp = %s\n", $self->ignore_fact_regexp;
    #printf " - \$key_fact_regexp = %s\n", $self->key_fact_regexp;
  }

  my %patternMemory = ();
  my $thisRubricLineNum = 0;
  my $thisResultsLineNum = 0;
  my $lastRubricMatchLine = -1;
  my $lastResultsMatchLine = -1;
  my $lastRubricMatchString = "";
  my $lastResultsMatchString = "";

  RUBRIC_LINE:
    while (<$rubricFH>) {
      my $rawRubricLine = $_;
      $self->consumeRubricLine($rawRubricLine)
          if $self->isa("PRT::AllLinesConsumer");
      my $thisRubricLineString = $self->preFormatRubricLine($rawRubricLine);
      ++$thisRubricLineNum;

      $self->preUseCheckRubricLine($thisRubricLineString);
      if ($self->enforceRubricLine()) {
        if ($self->useRubricLine($thisRubricLineString)) {
          my $resultLineCheckers
              = $self->getResultLineChecker($thisRubricLineString,
                                            \%patternMemory);
          if ($self->show_comparisons) {
            print "   - Rubric line $thisRubricLineNum is relevent\n";
            print "     Checkers:\n";
            foreach my $checker (@$resultLineCheckers) {
              print "      . ", $checker->blurb(), "\n";
            }
          }

          $self->preResultCheckRubricLine($thisRubricLineString);
          while (<$resultsFH>) {
            my $rawResultsLine = $_;
            $self->consumeResultsLine($rawResultsLine)
                if $self->isa("PRT::AllLinesConsumer");
            my $thisResultsLineString
                = $self->preFormatResultsLine($rawResultsLine);
            ++$thisResultsLineNum;
            print "      - Comparing to results line $thisResultsLineNum,
        \"$thisResultsLineString\"\n"
                if $self->show_comparisons>1;

            my $matched = 0;
          TRY_CHECKERS:
            foreach my $checker (@$resultLineCheckers) {
              if ($checker->check($thisResultsLineString)) {
                $matched = 1;
                last TRY_CHECKERS;
              } elsif ($self->unskippableResultsLine($thisResultsLineString,
                                                     $thisRubricLineString)) {
                print "FAILURE: reached results line $thisResultsLineNum;
  $thisResultsLineString
while searching for rubric line $thisRubricLineNum
  $thisRubricLineString";
                return 0;
              }
            }

            if ($matched) {
              print "      * Matched results line $thisResultsLineNum,
        \"$thisResultsLineString\"\n"
                  if $self->show_comparisons>1;
              print "        Matched results line $thisResultsLineNum\n"
                  if $self->show_comparisons==1;
              $lastRubricMatchLine = $thisRubricLineNum;
              $lastResultsMatchLine = $thisResultsLineNum;
              $lastRubricMatchString = $thisRubricLineString;
              $lastResultsMatchString = $thisResultsLineString;
              $self->postResultCheckRubricLine($thisRubricLineString);
              next RUBRIC_LINE;
            } else {
              print "        Not a match.\n" if $self->show_comparisons>1;
            }
          }

          print "FAILURE: could not find a match to rubric line $thisRubricLineNum
  $thisRubricLineString\n";
          $self->printLastMatch($lastRubricMatchLine, $lastResultsMatchLine,
                                $lastRubricMatchString,
                                $lastResultsMatchString);
          return 0;
        } else {
          print "   - Ignoring rubric line $thisRubricLineNum
     (useRubricLine returns false):\n     $thisRubricLineString\n"
              if $self->show_comparisons>1;
          $self->postResultCheckRubricLine($thisRubricLineString);
        }
      } else {
        print "   - Ignoring rubric line $thisRubricLineNum
     (enforceRubricLine returns false):\n     $thisRubricLineString\n"
            if $self->show_comparisons>1;
        $self->postResultCheckRubricLine($thisRubricLineString);
      }
    }

  ## If the comparo needs to see all of the result lines, we continue
  ## to read them until we get to the end of that file.
  if ($self->isa("PRT::AllLinesConsumer")) {
    while (<$resultsFH>) {
      $self->consumeResultsLine($_);
    }
  }

  print "  Exhausted rubric file; succeeding.\n" if $self->show_comparisons;
  return 1;
}

sub unskippableResultsLine {
  my $self = shift;
  my $resultsLine = shift;
  my $rubricLine = shift;

  return 0;
}

sub preUseCheckRubricLine {
  # my $self = shift;
  # my $rubricLine = shift;
}

sub preResultCheckRubricLine {
  # my $self = shift;
  # my $rubricLine = shift;
}

sub postResultCheckRubricLine {
  # my $self = shift;
  # my $rubricLine = shift;
}

sub printLastMatch {
  my $self = shift;
  croak "'$self' is not a '$CLASS'" unless ref($self) && $self->isa($CLASS);

  my ($lastRubricMatchLineNum, $lastResultsMatchLineNum,
      $lastRubricMatchLineString, $lastResultsMatchLineString) = @_;
  if ($lastRubricMatchLineNum > -1) {
    print "Last match was to rubric line $lastRubricMatchLineNum\n
  ${lastRubricMatchLineString}\nby results line $lastResultsMatchLineNum
$lastResultsMatchLineString\n";
  } else {
    print "No lines of the rubric had been matched.\n";
  }
}

sub commonLinePreformat {
  my $self = shift;
  my $line = shift;
  $line =~ s/\r?\n$//; # remove CR so UNIX can compare to Windows
  return $line;
}

sub preFormatRubricLine {
  my $self = shift;
  my $line = shift;
  return $self->commonLinePreformat($line);
}

sub useRubricLine {
  my $self = shift;
  my $line = shift;
  return 1;
}

sub getResultLineChecker {
  my $self = shift;
  my $line = shift;
  my $memory = shift;
  if ($self->{patternMatch}) {
    return [ new PRT::LineCheckers::StringMatchLineCheckerWithMemory
             ($line, $memory, $self) ];
  } else {
    return [ new PRT::LineCheckers::StringMatchLineChecker($line) ];
  }
}

sub preFormatResultsLine {
  my $self = shift;
  my $line = shift;
  return $self->commonLinePreformat($line);
}

sub enforceRubricLine {
  my $self = shift;
  if (@_) {
    $self->{enforceRubricLine} = shift;
  } else {
    return $self->{enforceRubricLine};
  }
}

sub getBlessing {
  my $self = shift;
  return new PRT::Blessings::VerbatimCopyBlessing();
}

1;

## POD USAGE
__END__

=head1 NAME

PRT::GenericLineComparo - A top-level class for line-based PRT comparos

=head1 SYNOPSIS

  use PRT::GenericLineComparo;

  my $comparo = new PRT::GenericLineComparo();

=head1 DESCRIPTION

TODO Fill in general introduction, major functionality

=head2 Typical usage by subclasses

TODO Fill in.

=head2 Pattern matching in rubrics

Some program output lines will have content which varies from run to
run, but which is consistent within one run. In particular, the names
associated with dynamically-created instances of ontology-derived
classes can be expected to vary over time since their names rely on
internal counters. We can require consistency of these names in
rubrics using a pattern-matching construct.  Any string of the form
C<?{{NAME}}> is taken to be a \emph{pattern} (the default delimiters
C<?{{> and C<}}> can be changed via the C<patternStartDelimRegex> and
C<patternEndDelimRegex> slots). The first time a pattern of a
particular name is encountered in a rubric, the system records the
text matching the pattern in the actual result text. Any future
references to that named pattern in the rubric are expected to match
the same text in the results. For example, a rubric with the lines

  NEW HostEventHypothesis instance ?{{h1}}
  UPDATED HostEventHypothesis instance ?{{h1}}

could successfully match the result lines

  NEW HostEventHypothesis instance record132
  UPDATED HostEventHypothesis instance record132

or

  NEW HostEventHypothesis instance record945
  UPDATED HostEventHypothesis instance record945

but would I<not> match the result lines

  NEW HostEventHypothesis instance record132
  UPDATED HostEventHypothesis instance record945

=head1 METHODS

TODO Fill in.
