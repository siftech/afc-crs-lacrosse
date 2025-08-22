=pod

=head1 NAME

PRT::AllLinesConsumer - Specifies the API for comparos which perform
analysis on every line of the rubric and results, separate from the
usual pairs-of-lines PRT comparison.

=cut

# ------------------------------------------------------------
# COPYRIGHT START
# Copyright (c) 2014, Smart Information Flow Technologies (SIFT).
# COPYRIGHT END
# ------------------------------------------------------------

package PRT::AllLinesConsumer;

sub initializeLinesConsumer {
  my $self = shift;
  my $agent = shift;
  my $testSpec = shift;
  return;
}

sub finalizeLinesConsumer {
  my $self = shift;
  return 1;
}

sub consumeRubricLine {
  my $self = shift;
  my $rubricLine = shift;
  return;
}

sub consumeResultsLine {
  my $self = shift;
  my $resultsLine = shift;
  return;
}

1;

__END__

=pod

=head1 SYNOPSIS

  use PRT::GenericLineComparo;
  use PRT::AllLinesConsumer;
  use parent qw(PRT::GenericLineComparo PRT::AllLinesConsumer);

  ## Override methods with useful activities.

  sub initializeLinesConsumer {
    my $self = shift;
    my $agent = shift;
    my $testSpec = shift;

    ## ...

    return;
  }

  sub finalizeLinesConsumer {
    my $self = shift;
    my @results;

    ## ...

    return @results;
  }

  sub consumeRubricLine {
    my $self = shift;
    my $rubricLine = shift;

    ## ...

    return;
  }

  sub consumeResultsLine {
    my $self = shift;
    my $resultsLine = shift;

    ## ...

    return;
  }

=head1 DESCRIPTION

This package defines the interface methods for comparos which perform
analysis on every line of the rubric and results, separate from the
usual pairs-of-lines PRT comparison.

One example of such comparo behavior is checking performance,
implemented by the mixin defined in L<PRT::PerfChecker>.

The C<initializeLinesConsumer> and C<finalizeLinesConsumer> methods
are called from the L<PRT::TestSpec>.

=over

=item C<initializeLinesConsumer>

This method allows the comparo or mixin to set up any data structures
for the lines consumption.  This method is not expected to return a
result.

=item C<finalizeLinesConsumer>

This method is called when all lines are consumed, and a pass/fail
judgment should be made.  It should returns a list of values.  The
first value is true for pass, false for fail.  The remaining values
are taken as a format string and values which, passed as arguments to
Perl's standard function sprintf, describe the test result. If the
first value is false, then the remaining arguments describe reason for
failure. If the first value is true, then the remaining values, if
they exist, consitute a warning.

=back

The C<consumeRubricLine> and C<consumeResultsLine> methods must be
called from comparos which are compatible with this all-lines
processing.  This behavior relies on a correct implementation in the
comparo, since it is the comparo which actually reads the rubric and
result values.

=over

=item C<consumeRubricLine>

This method consumes a line of the rubric, and does not return a
result.

=item C<consumeResultsLine>

This method consumes a line of the results log, and does not return a
result.

=back

Only comparos which inherit from this mixin will have these methods
called --- this allows ad hoc comparos which do not inherit from
the standard comparo superclasses in the PRT hierarchy to continue
to work.
