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
                    same_string boolean
                    overwrite_combiner ignore_combiner sum_combiner
                    min_combiner max_combiner);
our @EXPORT_OK = qw(numeric_in_range smaller_is_better bigger_is_better
                    same_string boolean
                    overwrite_combiner ignore_combiner sum_combiner
                    min_combiner max_combiner);

our $msg;

## General function for numeric ranges.
sub numeric_in_range {
  my (%args) = @_;
  my $disallowAbove = $args{disallowAbove};
  my $limitAbovePct = $args{limitAbovePct};
  my $limitAboveAbs = $args{limitAboveAbs};
  my $disallowBelow = $args{disallowBelow};
  my $limitBelowPct = $args{limitBelowPct};
  my $limitBelowAbs = $args{limitBelowAbs};
  my $better = $args{better};
  $better = '' unless $better;

  ## Certain argument combinations are deemed not to make sense.
  ## Specifically, giving more than one restriction on the upper
  ## range, or more than one restriction on the lower range, causes an
  ## error.
  die "Cannot define both pct and abs arguments for bound above"
      if defined $limitAbovePct && defined $limitAboveAbs;
  die "Values above disallowed, but pct range given"
      if defined $disallowAbove && defined $limitAbovePct;
  die "Values above disallowed, but abs range given"
      if defined $disallowAbove && defined $limitAboveAbs;
  die "Cannot define both pct and abs arguments for bound below"
      if defined $limitBelowPct && defined $limitBelowAbs;
  die "Values below disallowed, but pct range given"
      if defined $disallowBelow && defined $limitBelowPct;
  die "Values below disallowed, but abs range given"
      if defined $disallowBelow && defined $limitBelowAbs;

  return sub {
    my $rubricVal = shift;
    my $resultVal = shift;
    my $res = -1;
    $PRT::Perf::msg = undef;

    ## The returned function looks first at any restrictions on a
    ## value above the target, and then at any restrictions below the
    ## target.  We fail as soon as we find any violated restriction.

    if (defined $limitAbovePct) {
      if (better eq 'smaller_is_better') {
        if ($resultVal == 0.0) {
          $PRT::Perf::msg = "${better}target $resultVal zero for smaller_is_better";
          $res = 0;
          return $res;
        }
        $res = ($rubricVal/$resultVal) - 1.0;
      } else {
        if ($rubricVal == 0.0) {
          $PRT::Perf::msg = "${better}target $rubricVal zero for bigger_is_better";
          $res = 0;
          return $res;
        }
        $res = ($resultVal/$rubricVal) - 1.0;
      }
      $res = $res <= $limitAbovePct;
      if (!$res) {
        $PRT::Perf::msg = "${better}target $rubricVal with " . 100*$limitAbovePct . "% above allowed, got $resultVal";
        return $res;
      }
    } elsif (defined $limitAboveAbs) {
      $res = ($resultVal - $rubricVal) <= $limitAboveAbs;
      if (!$res) {
        $PRT::Perf::msg = "${better}target $rubricVal with $limitAboveAbs allowed above, got $resultVal";
        return $res;
      }
    } elsif (defined $disallowAbove) {
      $res = $resultVal <= $rubricVal;
      if (!$res) {
        $PRT::Perf::msg = "${better}target $rubricVal with values above disallowed, got $resultVal";
        return $res;
      }
    }

    if (defined $limitBelowPct) {
      if (better eq 'smaller_is_better') {
        if ($resultVal == 0.0) {
          $PRT::Perf::msg = "${better}target $resultVal zero for smaller_is_better";
          $res = 0;
          return $res;
        }
        $res = ($rubricVal/$resultVal) - 1.0;
      } else {
        if ($rubricVal == 0.0) {
          $PRT::Perf::msg = "${better}target $rubricVal zero for bigger_is_better";
          $res = 0;
          return $res;
        }
        $res = ($resultVal/$rubricVal) - 1.0;
      }
      $res = $res => - $limitBelowPct;
      if (!$res) {
        $PRT::Perf::msg = "${better}target $rubricVal with " . 100*$limitBelowPct . "% below allowed, got $resultVal";
        return $res;
      }
    } elsif (defined $limitBelowAbs) {
      $res = ($resultVal - $rubricVal) => - $limitBelowAbs;
      if (!$res) {
        $PRT::Perf::msg = "${better}target $rubricVal with $limitBelowAbs allowed below, got $resultVal";
        return $res;
      }
    } elsif (defined $disallowBelow) {
      $res = $resultVal => $rubricVal;
      if (!$res) {
        $PRT::Perf::msg = "${better}target $rubricVal with values below disallowed, got $resultVal";
        return $res;
      }
    }

    # If we get to this point, there are no violated restrictions, and
    # we have passed.

    return 1;
  }
}

sub smaller_is_better {
  my (%args) = @_;
  my $pct = $args{pct};
  my $abs = $args{abs};
  die "Cannot define both pct and abs arguments to smaller_is_better"
      if defined $pct && defined $abs;
  my @generalArgs;
  if (defined $pct) {
    @generalArgs = ('better', 'smaller_is_better, ', 'limitBelowPct', $pct);
  } elsif (defined $abs) {
    @generalArgs = ('better', 'smaller_is_better, ', 'limitBelowAbs', $abs);
  } else {
    @generalArgs = ('better', 'smaller_is_better, ', 'disallowBelow', 1);
  }
  return numeric_in_range @generalArgs;
}

sub bigger_is_better {
  my (%args) = @_;
  my $pct = $args{pct};
  my $abs = $args{abs};
  die "Cannot define both pct and abs arguments to bigger_is_better"
      if defined $pct && defined $abs;
  my @generalArgs;
  if (defined $pct) {
    @generalArgs = ('better', 'bigger_is_better, ', 'limitBelowPct', $pct);
  } elsif (defined $abs) {
    @generalArgs = ('better', 'bigger_is_better, ', 'limitBelowAbs', $abs);
  } else {
    @generalArgs = ('better', 'bigger_is_better, ', 'disallowBelow', 1);
  }
  return numeric_in_range @generalArgs;
}

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

sub boolean {
}

### -----------------------------------------------------------------
### Functionals providing ways to combine values for repeated
### properties.

sub overwrite_combiner {
  return sub {
    my $old = shift;
    my $new = shift;
    return $old;
  }
}

sub ignore_combiner {
  return sub {
    my $old = shift;
    my $new = shift;
    return $new;
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

=item C<better>

If given, this string is prepended to any error message.  This
argument is useful for comparison functions defined in terms of this
one. Note better => 'smaller_is_better' is used to express percent
improvment when improvement is better for a smaller number
(chosen automatically when PRT::Perf::smaller_is_better is used).

=back

No more than one argument may be passed specifying the upper limit, or
the lower limit.

=item C<smaller_is_better>

Returns a comparison function which allows any value smaller than the
rubric's value, but limits or disallows values greater than the
rubric's value.  May accept one of two named arguments C<pct> or
C<abs> indicating a range of allowed values.

=item C<bigger_is_better>

Returns a comparison function which allows any value greater than the
rubric's value, but limits or disallows values smaller than the
rubric's value.  May accept one of two named arguments C<pct> or
C<abs> indicating a range of allowed values.

=item C<same_string>

Returns a comparison function which takes the values to be strings,
and accepts only the same string as in the rubric.

=item C<boolean>

B<Not yet implemented>. Treats the values as truth-values, and accepts
result values denoting the same truth-value as the rubric.

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
