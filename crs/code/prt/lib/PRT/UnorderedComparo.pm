# ----------------------------------------
# Comparo for unordered output.
#
# This comparo ensures that all events of interest appear in output, regardless of order.
# You can use key_fact_regexp, ignore_fact_regexp, and case_sensitive as usual.
# strict=>0 (default) means that extra matching facts may occur in the results
# (but every rubric fact must have at least one matching result fact)
# strict=>1 means that there must be exactly the same number of key_fact occurrences in rubric and result.

package PRT::UnorderedComparo;

our @ISA = ("PRT::Base");

use strict;
use Carp;
use FileHandle;

use PRT::Base;

use PRT::Config qw( :default verboseMode );

our $AUTOLOAD;  # it's a package global
our $CLASS = "PRT::UnorderedComparo";

# names and initializer of fields
my %_FIELDS = (
               # public:
               # semi-public (from test spec)
               show_comparisons   => 1,
               ignore_fact_regexp => undef,
               case_sensitive =>1,
	       key_fact_regexp => ".*", # any line containing this pattern will be evaluated
	       strict =>0,	# extra key_fact_regexp matching lines in results are not allowed if this is nonzero

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
  my $fact_regexp = '';

  if (verboseMode()) {
    print "Configuration:\n";
    printf " - \$show_comparisons = %d\n", $self->show_comparisons;
    printf " - \$case_sensitive = %d\n", $self->case_sensitive;
    printf " - \$ignore_fact_regexp = %s\n", $self->ignore_fact_regexp;
    printf " - \$key_fact_regexp = %s\n", $self->key_fact_regexp;
    printf " - \$strict = %s\n", $self->strict;
  }

  if (!defined($self->key_fact_regexp)) {
    die("key_fact regexp is undef, this comparo is useless.\n");
  }
  else {
    # Wrap the whole expression so that we can refer to it if there
    # are no other groups specifed.
    $fact_regexp = "(" . $self->key_fact_regexp . ")";
  }

  # Record rubric statements, counting them up if repeated
  my %rubric_facts = ();

  print "Processing rubric lines:\n" if $self->show_comparisons;
  while(my $rubric_line = <$rubricFH>) {
    process_line($self, $fact_regexp, $rubric_line, \%rubric_facts,0);
  }
  
  print "Processing result lines:\n" if $self->show_comparisons;
  #  - decrement count when find in rubric; error if not in rubric or, if strict, too many are in result.
  while(my $results_line = <$resultsFH>) {
    if (!process_line($self, $fact_regexp, $results_line, \%rubric_facts,1)) {
	return 0;
	}
  }

	# at this point we know that all the results were in the rubric, but we need to make sure that
	# we indeed found all the rubric facts.  If not strict, all the values should be 0.
  my $allgood=1;
  for (keys %rubric_facts) {
    if ($self->strict && $rubric_facts{$_}>0) {
      print("FAILURE - Rubric fact [$_] not found in results enough times\n");
      $allgood=0;
    }
    elsif ($rubric_facts{$_}>0) {
      print("FAILURE - Rubric fact [$_] not found in results\n");
      $allgood=0;
    }
    elsif ($self->strict && $rubric_facts{$_}<0) {
      print("FAILURE - Rubric fact [$_] found in results too many times\n");
      $allgood=0;
    }
  }
  
  return $allgood;
}

sub process_line {
  my ($self,$fact_regexp,$line,$rubric_ref,$compare) = @_;
  my $ignore_fact_regexp = $self->ignore_fact_regexp;
  my $theFact=undef;
  $line =~ s/\r?\n$//; # remove CR so UNIX can compare to Windows

        if (defined($ignore_fact_regexp)
            && ( ($self->case_sensitive && ($line =~ /$ignore_fact_regexp/)) ||
                 (!($self->case_sensitive) && ($line =~ /$ignore_fact_regexp/i)) ) ) {
          print "      - Line [$line] matches ignore_fact_regexp, skipping\n" if $self->show_comparisons;
	  return 0;
        }

      if ( ($self->case_sensitive && $line =~ /$fact_regexp/) ||
           (!($self->case_sensitive) && $line =~ /$fact_regexp/i) ) {
        $theFact = $2;
        if (!defined($theFact)) {
          $theFact = $1; # Looks like no capturing group was present, use entire match.
        }
        print "   - Line [$line] matches the key_fact_regexp, yielding fact: [$theFact]\n"
          if $self->show_comparisons;
  
    if (!$compare) {	# record it into the rubric
    	if (defined($rubric_ref->{$theFact})) { 
	  $rubric_ref->{$theFact}++; 
	  print "	- Total occurrences: $rubric_ref->{$theFact}\n" if $self->show_comparisons;
	}
	else { $rubric_ref->{$theFact}=1; }
	return 1;
    }
    else {	# if the fact is in the rubric, if strict decrement count, else zero
    	if (defined($rubric_ref->{$theFact})) {
    	  if ($self->strict) { $rubric_ref->{$theFact}--; }
    	  else { $rubric_ref->{$theFact}=0; }
          return 1;
	}
	elsif ($self->strict) {
	  print "	FAILURE - Matching fact not in rubric: $theFact\n";
    	  return 0;
	}
	else {return 1;}
    }
  }
  return 1;	# ignore non-matching lines
}

sub record_result_fact {
  my $self = shift;
  croak "'$self' is not a '$CLASS'" unless ref($self) && $self->isa($CLASS);

  my $line = shift;
  my $result_fact_ref = shift;
  my $testline = $line;

  if (exists $result_fact_ref->{$line}) {
    return 1;
  }

  my $pattern = $self->key_fact_regex;
  if($testline =~ /$pattern/) {
    $result_fact_ref->{$line} = undef;
    return 1;
  }
  return 0;
}

1;
