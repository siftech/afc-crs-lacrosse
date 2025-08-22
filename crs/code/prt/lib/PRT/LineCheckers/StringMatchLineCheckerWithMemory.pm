# ------------------------------------------------------------
# COPYRIGHT START
# Copyright (c) 2014, Smart Information Flow Technologies (SIFT).
# COPYRIGHT END
# ------------------------------------------------------------
package PRT::LineCheckers::StringMatchLineCheckerWithMemory;
our @ISA = ("PRT::Base");
use strict;
use Carp;
use FileHandle;
use PRT::Base;

# names and initializer of fields
my %_FIELDS = (
  show_comparisons => 0,
    );
# Class-level return list of fields
sub FIELDS() { return \%_FIELDS; }

sub new {
  my $class = shift;
  my $target = shift;
  my $memory = shift;
  my $comparator = shift;
  my $self = $class->SUPER::new(@_);

  my $targetSrc = "$target";
  my $regex = '^';
  my @names=();

  my $patternPattern = $comparator->{patternStartDelimRegex}
      . '(' . $comparator->{patternNameRegex} . ')'
      . $comparator->{patternEndDelimRegex};
  my $grabPattern = '(' . $comparator->{matchPattern} . ')';
  my $i=0;

  # print "Preparing rublic line \"$target\"\n";
  while ($targetSrc =~ s/$patternPattern//p) {
    # print " - Literal \"${^PREMATCH}\", name \"$1\"\n";
    $names[$i++] = $1;
    $regex .= "\Q${^PREMATCH}\E" . $grabPattern;
    # print "   regex to \"$regex\"\n";
    $targetSrc = ${^POSTMATCH};
  }
  $regex .= "\Q$targetSrc\E\$";
  # print " - final regex \"$regex\"\n";

  $self->{matchNames} = \@names;
  $self->{regex} = $regex;
  $self->{memory} = $memory;

  # return the new object
  return $self;
}

sub check {
  my $self = shift;
  my $string = shift;
  my $regex = $self->{regex};
  my $names = $self->{matchNames};
  my $memory = $self->{memory};
  # print "** Matching \"$string\" against regex \"$regex\"\n";
  my $result = $string =~ m/$regex/;
  # print "** * $result\n";
  my $matches = matchVector();
  # print "** * ", $#{$matches}, "\n";

  for(my $i=0; $i<=$#{$matches}; ++$i) {
    my $name = $names->[$i];
    my $match = $matches->[$i];
    # print "** ** Checking word $i given $match against $name.\n";

    if (defined $memory->{$name}) {
      # print "** ** ** $name defined as $memory->{$name}\n";
      $result &= ($memory->{$name} eq $match);
    } else {
      # print "** ** ** Setting $name to \"$match\"\n";
      $memory->{$name} = $match;
    }
  }

  print "     ** Comparing string \"$string\"
        to regex \"$regex\":
        result: $result\n"
        if $self->show_comparisons();
  return $result;
}

sub blurb {
  my $self = shift;
  return "(eq \"" . $self->{regex} . "\")";
}

sub matchVector {
  my @result=();
  push @result, $1  if defined $1;
  push @result, $2  if defined $2;
  push @result, $3  if defined $3;
  push @result, $4  if defined $4;
  push @result, $5  if defined $5;
  push @result, $6  if defined $6;
  push @result, $7  if defined $7;
  push @result, $8  if defined $8;
  push @result, $9  if defined $9;
  # push @result, ${10}  if defined ${10};
  # push @result, ${11}  if defined ${11};
  # push @result, ${12}  if defined ${12};
  # push @result, ${13}  if defined ${13};
  # push @result, ${14}  if defined ${14};
  # push @result, ${15}  if defined ${15};
  # push @result, ${16}  if defined ${16};
  # push @result, ${17}  if defined ${17};
  # push @result, ${18}  if defined ${18};
  # push @result, ${19}  if defined ${19};
  return \@result;
}
