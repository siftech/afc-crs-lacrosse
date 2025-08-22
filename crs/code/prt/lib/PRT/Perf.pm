=pod

=head1 NAME

PRT::Perf - A library of exported comparison and combination functions
for use in performance-checking specifications.

=head1 SYNOPSIS

  use PRT::Perf;

=head1 DESCRIPTION

This package exports two groups of functions, I<comparison functions>
and I<combination functions>, all of which themselves return
functions.  The former are used to produce function values for
C<compFn> fields (and the default-setting fields in the various hashes
enclosing a performance-checking spec) in performance checking
specifications; the latter, for C<combineFn> fields.

Unless noted otherwise, the functions defined in the package take
named arguments.

=cut

# ------------------------------------------------------------
# COPYRIGHT START
# Copyright (c) 2014, Smart Information Flow Technologies (SIFT).
# COPYRIGHT END
# ------------------------------------------------------------

package PRT::Perf;
require Exporter;
our @ISA = qw(Exporter);
our @EXPORT    = qw(numeric_in_range smaller_is_better bigger_is_better
                    same_number same_string boolean
                    overwrite_combiner ignore_combiner sum_combiner
                    min_combiner max_combiner);
our @EXPORT_OK = qw(numeric_in_range smaller_is_better bigger_is_better
                    same_number same_string boolean
                    overwrite_combiner ignore_combiner sum_combiner
                    min_combiner max_combiner);

our $msg;

=pod

=head2 Comparison functions

=over

=item C<numeric_in_range>

Returns a comparison function for numeric property values, requiring
that the result value be within a certain range of the rubric's
value. Takes up to three of the following named arguments:

=over

=item C<disallowBelow>

Values below the rubric's value are not allowed.

=item C<limitBelowPct>

Values a certain percentage below the rubric's value are not allowed.

=item C<limitBelowAbs>

Values below a fixed amount over the rubric's value are not allowed.

=item C<disallowAbove>

Values above the rubric's value are not allowed.

=item C<limitAbovePct>

Values a certain percentage above the rubric's value are not allowed.

=item C<limitAboveAbs>

Values above a fixed amount over the rubric's value are not allowed.

=item C<pctIsImprovement>

By default, percentage arguments are taken to define an acceptable
interval for specimen result values in terms of a percentage
above/below the baseline rubric value.

If this argument is present and true, then percentage limits will
taken to describe the percentage of improvement instead.  In
particular, this flag is set in the translation of the
C<bigger_is_better> and C<smaller_is_better> convenience functions.

This argument is only relevant to the C<limitBelowPct> and
C<limitAllowPct> arguments.

=item C<prefix>

If given, this string is prepended to any error message.  This
argument is useful for comparison functions defined in terms of this
one.

=back

No more than one argument may be passed specifying the upper limit, or
the lower limit.

=cut

sub numeric_in_range {
  my (%args) = @_;
  my $disallowAbove = $args{disallowAbove};
  my $limitAbovePct = $args{limitAbovePct};
  my $limitAboveAbs = $args{limitAboveAbs};
  my $disallowBelow = $args{disallowBelow};
  my $limitBelowPct = $args{limitBelowPct};
  my $limitBelowAbs = $args{limitBelowAbs};
  my $pctIsImproved = $args{pctIsImproved};
  my $prefix = $args{prefix};
  $prefix = '' unless $prefix;

  ## Certain argument combinations are deemed not to make sense.
  ## Specifically, giving more than one restriction on the upper
  ## range, or more than one restriction on the lower range, causes an
  ## error.
  die "Values above disallowed, but pct and abs ranges given"
      if defined $disallowAbove
      && defined $limitAbovePct && defined $limitAboveAbs;
  die "Cannot define both pct and abs arguments for bound above"
      if defined $limitAbovePct && defined $limitAboveAbs;
  die "Values above disallowed, but pct range given"
      if defined $disallowAbove && defined $limitAbovePct;
  die "Values above disallowed, but abs range given"
      if defined $disallowAbove && defined $limitAboveAbs;
  die "Values below disallowed, but pct and abs ranges given"
      if defined $disallowBelow
      && defined $limitBelowPct && defined $limitBelowAbs;
  die "Cannot define both pct and abs arguments for bound below"
      if defined $limitBelowPct && defined $limitBelowAbs;
  die "Values below disallowed, but pct range given"
      if defined $disallowBelow && defined $limitBelowPct;
  die "Values below disallowed, but abs range given"
      if defined $disallowBelow && defined $limitBelowAbs;

  return sub {
    my $rubricVal = shift;
    my $resultVal = shift;
    $PRT::Perf::msg = undef;
    my $res=1;

    ## First check where the rubric (baseline) and result (specimen)
    ## values are with respect to each other.  If the specimen is
    ## greater than the baseline, we must check it against any limits
    ## we have for values above the rubric value.

    if ($resultVal > $rubricVal) {
      if (defined $limitAbovePct) {
        my $quotient;
        if ($pctIsImproved) { # TODO Tom check this calculation.
          $quotient = ($resultVal/$rubricVal)-1;
          $res = $quotient <= $limitAbovePct;
        } else {
          $quotient = ($resultVal/$rubricVal)-1;
          print "  - checking for limitAbovePct\n    rubricVal=$rubricVal, resultVal=$resultVal\n    (resultVal/rubricVal)-1=$quotient, cap=$limitAbovePct, cmp=", ($quotient <= $limitAbovePct), "\n"
              if PRT::Config::debugMode()>1;
          $res = $quotient <= $limitAbovePct;
        }
        $PRT::Perf::msg = "${prefix} target $rubricVal with " . 100*$limitAbovePct . "\% above allowed, got $resultVal"
            unless $res;
      } elsif (defined $limitAboveAbs) {
        $res = ($resultVal - $rubricVal) <= $limitAboveAbs;
        $PRT::Perf::msg = "${prefix} target $rubricVal with $limitAboveAbs allowed above, got $resultVal"
            unless $res;
      } elsif (defined $disallowAbove) {
        $res = $resultVal <= $rubricVal;
        $PRT::Perf::msg = "${prefix} target $rubricVal with values above disallowed, got $resultVal"
            unless $res;
      }

    ## If the specimen is less than the baseline, we must check it
    ## against any limits we have for values below the rubric value.
    } elsif ($resultVal < $rubricVal) {
      if (defined $limitBelowPct) {
        my $quotient;
        if ($pctIsImproved) { # TODO Tom check this calculation.
          $quotient = 1-($resultVal/$rubricVal);
          $res = $quotient <= $limitBelowPct;
        } else {
          $quotient = 1-($resultVal/$rubricVal);
          $res = $quotient <= $limitBelowPct;
        }
        $PRT::Perf::msg = "${prefix} target $rubricVal allowing " . (100*$limitBelowPct) . " percent below, got $resultVal"
            unless $res;
      } elsif (defined $limitBelowAbs) {
        $res = ($rubricVal - $resultVal) <= $limitBelowAbs;
        $PRT::Perf::msg = "${prefix} target $rubricVal with $limitBelowAbs allowed below, got $resultVal"
            unless $res;
      } elsif (defined $disallowBelow) {
        $res = $resultVal <= $rubricVal;
        $PRT::Perf::msg = "${prefix} target $rubricVal with values below disallowed, got $resultVal"
            unless $res;
      }
    }

    # Otherwise the result and rubric values are the same, which we're
    # happy to see.
    return $res;
  }
}

=pod

=item C<smaller_is_better>

Returns a comparison function which allows any value smaller than the
rubric's value, but limits or disallows values greater than the
rubric's value.  May accept one of two named arguments C<pct> or
C<abs> indicating a range of allowed values.

=cut

sub smaller_is_better {
  my (%args) = @_;
  my $pct = $args{pct};
  my $abs = $args{abs};
  die "Cannot define both pct and abs arguments to smaller_is_better"
      if defined $pct && defined $abs;
  my @generalArgs;
  if (defined $pct) {
    @generalArgs = ('prefix', 'smaller_is_better, ', 'limitAbovePct', $pct,
                    'pctIsImproved', 1);
  } elsif (defined $abs) {
    @generalArgs = ('prefix', 'smaller_is_better, ', 'limitAboveAbs', $abs);
  } else {
    @generalArgs = ('prefix', 'smaller_is_better, ', 'disallowAbove', 1);
  }
  return numeric_in_range @generalArgs;
}

=pod

=item C<bigger_is_better>

Returns a comparison function which allows any value greater than the
rubric's value, but limits or disallows values smaller than the
rubric's value.  May accept one of two named arguments C<pct> or
C<abs> indicating a range of allowed values.

=cut

sub bigger_is_better {
  my (%args) = @_;
  my $pct = $args{pct};
  my $abs = $args{abs};
  die "Cannot define both pct and abs arguments to bigger_is_better"
      if defined $pct && defined $abs;
  my @generalArgs;
  if (defined $pct) {
    @generalArgs = ('prefix', 'bigger_is_better, ', 'limitBelowPct', $pct,
                    'pctIsImproved', 1);
  } elsif (defined $abs) {
    @generalArgs = ('prefix', 'bigger_is_better, ', 'limitBelowAbs', $abs);
  } else {
    @generalArgs = ('prefix', 'bigger_is_better, ', 'disallowBelow', 1);
  }
  return numeric_in_range @generalArgs;
}

=pod

=item C<same_number>

Returns a comparison function which takes the values to be numbers,
and accepts only the same number as in the rubric.

=cut

sub same_number {
  return sub {
    my $rubricVal = shift;
    my $resultVal = shift;
    $PRT::Perf::msg = undef;
    my $res = $rubricVal == $resultVal;
    $PRT::Perf::msg
        = "target $rubricVal, actual $resultVal, required same_number"
        unless $res;
    return $res;
  };
}

=pod

=item C<same_string>

Returns a comparison function which takes the values to be strings,
and accepts only the same string as in the rubric.

=cut

sub same_string {
  return sub {
    my $rubricVal = shift;
    my $resultVal = shift;
    $PRT::Perf::msg = undef;
    my $res = $rubricVal eq $resultVal;
    $PRT::Perf::msg = "target '$rubricVal', actual '$resultVal' but same_string"
        unless $res;
    return $res;
  };
}

=pod

=item C<boolean>

B<Not yet implemented>. Treats the values as truth-values, and accepts
result values denoting the same truth-value as the rubric.

=cut

sub boolean {
}

### -----------------------------------------------------------------
### Functionals providing ways to combine values for properties
### repeated in the rubric or result.

=pod

=back

=head2 Combination functions

=over

=item C<overwrite_combiner>

Returns the default combination function, which simply overwrites an
earlier value for a property with the newer one.

=item C<ignore_combiner>

Returns a combination function which ignores subsequent values for the
same property.

=item C<sum_combiner>

Returns a combination function which retains the sum of multiple
values for the same property.

=item C<min_combiner>

Returns a combination function which retains the minimum of multiple
values for the same property.

=item C<max_combiner>

Returns a combination function which retains the maximum of multiple
values for the same property.

=back

=cut

sub overwrite_combiner {
  return sub {
    my $old = shift;
    my $new = shift;
    return $new;
  }
}

sub ignore_combiner {
  return sub {
    my $old = shift;
    my $new = shift;
    return $old;
  }
}

sub sum_combiner {
  return sub {
    my $old = shift;
    my $new = shift;
    return $old+$new;
  }
}

sub min_combiner {
  return sub {
    my $old = shift;
    my $new = shift;
    return $old<$new ? $old : $new;
  }
}

sub max_combiner {
  return sub {
    my $old = shift;
    my $new = shift;
    return $old>$new ? $old : $new;
  }
}

1;

__END__
