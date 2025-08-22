#!/usr/bin/env perl

my $matching = 0;
my $sending = 0;
my $processing = 0;
my $parens = 0;
my $acc = "";

sub count_parens ($) {
  $_ = shift;
  my $opens = $_ =~ tr/\(//;
  my $closes = $_ =~ tr/\)//;
  # print STDERR "$_ has $opens open parens and $closes closes\n";
  my $parens = $opens - $closes;
  return $parens;
}

while (<>) {
  chomp;
  if ($matching) {
    my $new;
    my $postfix = "";
    # end of pattern
    if ( m/^\s*(.*)((\s+((to)|(from)))(\s+.*))$/ ) {
        $new = $1;
        $postfix = $2;
        # print STDERR "End of match\n";
        # print STDERR "Accumulating $new to message.\n";
        # print STDERR "Postfix match is $postfix\n";
    } else {
      m/^\s*(.*)$/ or die "Can't accumulate from $_\n";
      $new = $1;
      # print STDERR "Accumulating $new to message.\n";
    }
    $acc = $acc . $new . "\n";
    my $delta_parens = count_parens($new);
    # print STDERR "$parens at start ";
    $parens = $parens + $delta_parens;
    # print STDERR "$parens at end.\n";
    if ($postfix || $parens == 0) {
      # done matching message
      # print STDERR "Ending message at $_\n";
      die "Unexpected message termination: $_"
          unless $postfix && ( $parens == 0 );
      print "Received message:\n" if $processing;
      print "Sending message:\n" if $sending;
      print "$acc\n\n";
      $acc = "";
      $matching = 0;
      $processing = 0;
      $sending = 0;
      $parens = 0;
    }
  } elsif (m/MSG:\s+((Sending)|(Processing))\s+msg\s+(\(.*)$/) {
    $matching = 1;
    $acc = "$4\n";
    $parens = count_parens($acc);
    # print STDERR "Starting match with $acc; paren count = $parens\n";
    if ( $1 eq "Sending" ) {
      $sending = 1;
      $processing = 0;
    } elsif ( $1 eq "Processing" ) {
      $sending = 0;
      $processing = 1;
    } else {
      die "Unknown message operation, $2";
    }
  }
  # nothing to see here, move along
}


