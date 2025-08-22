# ------------------------------------------------------------
# COPYRIGHT START
# Copyright (c) 2014, Smart Information Flow Technologies (SIFT).
# COPYRIGHT END
# ------------------------------------------------------------
# PRT::SingleAsdfTestOp
##
#
# COPYRIGHT START
# Copyright (c) 2011, Smart Information Flow Technologies (SIFT).
# Developed with the sponsorship of the Defense Advanced Research Projects Agency (DARPA).
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this data, including any software or models in source or binary form, as well
# as any drawings, specifications, and documentation (collectively "the Data"),
# to deal in the Data without restriction, including without limitation the
# rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
# sell copies of the Data, and to permit persons to whom the Data is furnished to
# do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Data.
#
# THE DATA IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
# PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS,
# SPONSORS, DEVELOPERS, CONTRIBUTORS, OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
# OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE DATA OR THE USE OR
# OTHER DEALINGS IN THE DATA.
# COPYRIGHT END
#
##
# Holds config and common stuff for PRT tests for a single Lisp ASDF
# test-op.
##

package PRT::SingleAsdfTestOp;
our $AUTOLOAD;  # it's a package global
our $CLASS = "PRT::SingleAsdfTestOp";
use strict;
use PRT::LispNstComparo;
use FindBin;

require Exporter;
our @ISA = qw(Exporter);
# Breaking all the rules and exporting subroutines!
our @EXPORT      = qw(make_testop_test_spec);
our @EXPORT_OK   = qw(make_testop_test_spec);
our %EXPORT_TAGS = (default => [@EXPORT], modes => [@EXPORT_OK]);

sub make_testop_test_spec {
  my %args = ();
  while ($#_ > -1) {
    my $key = shift;
    my $value = shift;
    $args{$key} = $value;
  }

  my $targetSystem = $args{targetSystem};
  my $comparo = $args{comparo};

  my $description = $args{description};
  my $keywords = $args{keywords};
  my $defaultIgnore = $args{defaultIgnore};

  my $asdFinderDir = $args{asdFinderDir};
  my $asdfDirectories = $args{asdfDirectories};
  my $asdfRootDirectories = $args{asdfRootDirectories};
  my $lispImage = $args{lispImage};
  my $skipSetup = $args{skipSetup};

  my $preTest = $args{preTest};
  my $postTest = $args{postTest};

  my $force = $args{force};

  die "Must provide targetSystem"    unless defined $targetSystem;
  $comparo = new PRT::LispNstComparo unless defined $comparo;
  die "Need asdFinderDir for asdfRootDirectories"
      if (defined $asdfRootDirectories) && !(defined $asdFinderDir);

  $description = "Run the ASDF test-op on $targetSystem."
      unless defined $description;
  $keywords = [ qw / prt / ]  unless defined $keywords;
  $defaultIgnore = 0  unless defined $defaultIgnore;

  my $lispCalls = "";

  if (!$skipSetup) {
    $lispCalls .= "(require :asdf)\n(setf *print-readably* nil)\n";

    if (defined $asdfRootDirectories) {
      $lispCalls .= "(let ((asdf:*central-registry*\n       (list #p\"$FindBin::RealBin/$asdFinderDir\")))\n  (asdf:load-system :asd-finder))\n";
      $lispCalls .= "(setf asdf:*central-registry*\n      (append asdf:*central-registry*\n              (asd-finder:asd-finder (list";
      foreach my $dir (@{$asdfRootDirectories}) {
        $lispCalls .= " #p\"$FindBin::RealBin/$dir\"";
      }
      $lispCalls .= "))))\n";
    }

    foreach my $dir (@{$asdfDirectories}) {
      $lispCalls .= "(push #p\"$FindBin::RealBin/$dir\" asdf:*central-registry*)\n";
    }
  }

  $lispCalls .= ("\n;; --------------------\n\n"
                 . $preTest . "\n\n;; --------------------\n")
      if defined $preTest;

  $lispCalls .= "(asdf:test-system :$targetSystem";
  $lispCalls .= " :force t" if $force;
  $lispCalls .= ")\n";

  $lispCalls .= "\n;; --------------------\n\n" . $postTest . "\n"
      if defined $postTest;

  ## Comment out for normal operations, >1 for everything, 1 for some.
  ## $comparo->show_comparisons(2);

  my $test = {
    # name => $name,
    description => $description,
    keywords => $keywords,
    defaultIgnore => $defaultIgnore,

    agents => [
      {
        name         => "script",
        logfile      => "script.lisp",
        description  => "Build Lisp script",
        command      => sub { my @cline = ("/usr/bin/env", "echo", $lispCalls);
                              return @cline; },
        # timeout      => $prism_timeout,
        exit_code    => 0,
        comparo      => undef, # This is just input file prep.
        complete_before_continuing => 1,
      },

      {
        name         => "lisp",
        description  => "Run the Lisp script",
        command      => sub {
          my $agent_spec = shift;
          my @result = ("alisp");
          push @result, '-I', $lispImage if defined $lispImage;
          push @result, '-qq', '-batch';
          push @result, '-L', $agent_spec->testspec->results_dir.'/script.lisp';
          return @result;
        },
        # timeout      => $arff_timeout,
        exit_code    => 0,
        complete_before_continuing => 1,
        comparo      => $comparo,
      }
    ]
  };

  # Always end with a reference to the test specification hash reference.
  return $test;
}

1;
