# PRT::CulpritsGlobals
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
# Holds config and common stuff for Culprits testing in PRT
##

package PRT::META::CulpritsGlobals;
our $AUTOLOAD;  # it's a package global
our $CLASS = "PRT::CulpritsGlobals";

my $waffles_dir = $ENV{WAFFLES_DIR};
die "Set WAFFLES_DIR to the local repos"
    unless defined $waffles_dir;
die "No such waffles directory $waffles_dir"
    unless -d $waffles_dir;

my $culprits_dir = $ENV{CULPRITS_DIR};
die "Set CULPRITS_DIR to the local repos"
    unless defined $culprits_dir;
die "No such culprits directory $culprits_dir"
    unless -d $culprits_dir;

use strict;
use PRT::FullArffComparo;

require Exporter;
our @ISA = qw(Exporter);
# Breaking all the rules and exporting subroutines!
our @EXPORT      = qw(make_culprits_test_spec);
our @EXPORT_OK   = qw(make_culprits_test_spec);
our %EXPORT_TAGS = (default => [@EXPORT], modes => [@EXPORT_OK]);

sub make_culprits_test_spec {
  my %args = ();
  while ($#_ > -1) {
    my $key = shift;
    my $value = shift;
    $args{$key} = $value;
  }

  my $description = $args{description};
  my $keywords = $args{keywords};
  my $defaultIgnore = $args{defaultIgnore};
  my $prism_property = $args{prism_property};
  my $prism_model = $args{prism_model};
  my $sample_size = $args{sample_size};
  my $sample_seed = $args{sample_seed};
  my $sample_pathlen = $args{sample_pathlen};
  my $waffles_leafthresh = $args{waffles_leafthresh};
  my $waffles_maxlevels = $args{waffles_maxlevels};
  my $prism_timeout = $args{prism_timeout};
  my $arff_timeout = $args{arff_timeout};
  my $dtree_timeout = $args{dtree_timeout};
  my $plot_timeout = $args{plot_timeout};
  my $simpathv = $args{simpathv};
  my $track_column_regexp = $args{track_column_regexp};
  my $track_column_namelist = $args{track_column_namelist};
  my $nominal_regexp = $args{nominal_regexp};
  my $column_val_acc = $args{column_value_accumulator};

  die "Must specify prism_property" unless defined $prism_property;
  die "Must specify prism_model" unless defined $prism_model;
  die "No such prism_model file $prism_model" unless -e $prism_model;
  warn "sample_seed not specified"  unless defined $sample_seed;
  $sample_size = 6  unless defined $sample_size;
  $sample_seed = 102  unless defined $sample_seed;
  $sample_pathlen = 1000000  unless defined $sample_pathlen;
  $waffles_leafthresh = 2  unless defined $waffles_leafthresh;
  $waffles_maxlevels = 0  unless defined $waffles_maxlevels;
  $prism_timeout = 60*10  unless defined $prism_timeout; # 10 min.:
                                                         # PRISM can
                                                         # be slow.
  $arff_timeout = 60*5  unless defined $arff_timeout;
  $dtree_timeout = 60*3  unless defined $dtree_timeout;
  $plot_timeout = 60*3  unless defined $plot_timeout;

  $description = "Run a PRISM sim, and digest its output into ARFF files etc."
      unless defined $description;
  $keywords = [ qw / prism sim arff / ]  unless defined $keywords;
  $defaultIgnore = 0  unless defined $defaultIgnore;

  my $samples_or_method;
  if ($sample_size == 0) {
    $samples_or_method = '-simmethod';
    $sample_size = 'sprt';
  } else {
    $samples_or_method = '-simsamples';
  }

  my @prism_proc_args = ();
  if (defined $track_column_regexp) {
    foreach my $name (@$track_column_namelist) {
      push @prism_proc_args, "-C", $name;
    }
  }
  push @prism_proc_args, '-A', $column_val_acc
      if defined $column_val_acc;
  push @prism_proc_args, "-R", $track_column_regexp
      if defined $track_column_regexp;


  my $test = {
    description => $description,
    keywords => $keywords,
    defaultIgnore => $defaultIgnore,

    agents => [
      {
        name         => "prism",
        description  => "Run PRISM in -sim mode",
        executable   => "prism",
        timeout      => $prism_timeout,
        add_args     => [ # The model --- always the first argument.
                          $prism_model,
                          '-csl',

                          # The property to examine in the sim:
                          $prism_property,
                          '-sim',

                          # Number of simulations, or use SPRT.
                          $samples_or_method, $sample_size,

                          # Seed the randomizer for reproduceable
                          # results.
                          '-simseed', $sample_seed,

                          ## Don't need to give this for time-limited
                          ## properties.
                          # Maximum allowed path length.
                          # '-simpathlen', $sample_pathlen,
            ],
        exit_code    => 0,
        comparo      => undef, # This is going to be a huge file, which
                               # if incorrect will make the subsequent
                               # artifacts incorrect as well.
                               # Therefore, we won't waste time with a
                               # comparo for it --- we'll catch errors
                               # in the much smaller ARFF files.
        complete_before_continuing => 1,
      },

      {
        name         => "mval-nom-arff",
        logfile      => "mval-nom.arff", # Because waffles will object
                                         # to the .log file extension.
        description  => "Build multivalued-nominal style ARFF",
        command      => sub {
          my $agent_spec = shift;
          my @result = ("$culprits_dir/src/prism-sim-proc",
                        '-s', 'parc-arff-multival-nominal',
                        '-i', $agent_spec->testspec->results_dir.'/prism.log');
          foreach my $m (@prism_proc_args) { push @result, $m; }
          return @result;
        },
        timeout      => $arff_timeout,
        exit_code    => 0,
        complete_before_continuing => 1,
        comparo      => new PRT::FullArffComparo(),
      },

      {
        name         => "asym-bin-arff",
        logfile      => "asym-bin.arff", # Because waffles will object
                                         # to the .log file extension.
        description  => "Build asymmetric binary style ARFF",
        command      => sub {
          my $agent_spec = shift;
          my @result = ("$culprits_dir/src/prism-sim-proc",
                        '-s', 'parc-arff-asymmetric-binary',
                        '-i', $agent_spec->testspec->results_dir.'/prism.log');
          foreach my $m (@prism_proc_args) { push @result, $m; }
          if (defined $nominal_regexp) {
            push @result, '-N', $nominal_regexp;
          }
          return @result;
        },
        timeout      => $arff_timeout,
        exit_code    => 0,
        complete_before_continuing => 1,
        comparo      => new PRT::FullArffComparo(),
      },

      {
        name         => "mval-nom-json-dtree",
        logfile      => "mval-nom-dtree.json",
        description  => "Build a JSON model of the decision tree constructed by waffles from the multivalued-nominal style ARFF",
        command      => sub {
          my $agent_spec = shift;
          return "$waffles_dir/bin/waffles_learn",
          "train", $agent_spec->testspec->results_dir.'/mval-nom.arff', "decisiontree",
          "-leafthresh", $waffles_leafthresh,
          "-maxlevels", $waffles_maxlevels;
        },
        timeout      => $dtree_timeout,
        exit_code    => 0,
        complete_before_continuing => 1,
        comparo      => undef,
      },

      {
        name         => "asym-bin-json-dtree",
        logfile      => "asym-bin-dtree.json",
        description  => "Build a JSON model of the decision tree constructed by waffles from the asymmetric-binary style ARFF",
        command      => sub {
          my $agent_spec = shift;
          return "$waffles_dir/bin/waffles_learn",
          "train", $agent_spec->testspec->results_dir.'/asym-bin.arff', "decisiontree",
          "-leafthresh", $waffles_leafthresh,
          "-maxlevels", $waffles_maxlevels;
        },
        timeout      => $dtree_timeout,
        exit_code    => 0,
        complete_before_continuing => 1,
        comparo      => undef,
      },

      {
        name         => "mval-nom-json-dtree-plot",
        logfile      => "mval-nom-dtree.txt",
        description  => "Plot the JSON model of the decision tree constructed by waffles from the multivalued-nominal style ARFF",
        command      => sub {
          my $agent_spec = shift;
          return "$waffles_dir/bin/waffles_plot", "printculprits",
          $agent_spec->testspec->results_dir.'/mval-nom-dtree.json',
          $agent_spec->testspec->results_dir.'/mval-nom.arff';
        },
        timeout      => $plot_timeout,
        exit_code    => 0,
        complete_before_continuing => 0,
        comparo      => new PRT::LineComparo(),
      },

      {
        name         => "asym-bin-json-dtree-plot",
        logfile      => "asym-bin-dtree.txt",
        description  => "Plot the JSON model of the decision tree constructed by waffles from the multivalued-nominal style ARFF",
        command      => sub {
          my $agent_spec = shift;
          return "$waffles_dir/bin/waffles_plot", "printculprits",
          $agent_spec->testspec->results_dir.'/asym-bin-dtree.json',
          $agent_spec->testspec->results_dir.'/asym-bin.arff';
        },
        timeout      => $plot_timeout,
        exit_code    => 0,
        complete_before_continuing => 0,
        comparo      => new PRT::LineComparo(),
      } ]
  };

  if (defined $simpathv) {
    # The -simpathv option isn't the default for now --- need a bit
    # more to get it working.
    push @{$test->{agents}[0]{add_args}}, '-simpathv', $simpathv, 'stdout';
  } else {
    # Verbose mode gives us the output we need, but it's just kind of
    # temporary..
    push @{$test->{agents}[0]{add_args}}, '-v';
  }

  # Always end with a reference to the test specification hash reference.
  return $test;
}

sub make_analytic_culprits_test_spec {
  my %args = ();
  while ($#_ > -1) {
    my $key = shift;
    my $value = shift;
    $args{$key} = $value;
  }

  my $description = $args{description};
  my $keywords = $args{keywords};
  my $defaultIgnore = $args{defaultIgnore};
  my $prism_property = $args{prism_property};
  my $prism_model = $args{prism_model};
  my $sample_size = $args{sample_size};
  my $sample_seed = $args{sample_seed};
  my $sample_pathlen = $args{sample_pathlen};
  my $waffles_leafthresh = $args{waffles_leafthresh};
  my $waffles_maxlevels = $args{waffles_maxlevels};
  my $prism_timeout = $args{prism_timeout};
  my $arff_timeout = $args{arff_timeout};
  my $dtree_timeout = $args{dtree_timeout};
  my $plot_timeout = $args{plot_timeout};
  my $simpathv = $args{simpathv};

  die "Must specify prism_property" unless defined $prism_property;
  die "Must specify prism_model" unless defined $prism_model;
  die "No such prism_model file $prism_model" unless -e $prism_model;
  warn "sample_seed not specified"  unless defined $sample_seed;
  $sample_size = 6  unless defined $sample_size;
  $sample_seed = 102  unless defined $sample_seed;
  $sample_pathlen = 1000000  unless defined $sample_pathlen;
  $waffles_leafthresh = 2  unless defined $waffles_leafthresh;
  $waffles_maxlevels = 0  unless defined $waffles_maxlevels;
  $prism_timeout = 60*10  unless defined $prism_timeout; # 10 min.:
                                                         # PRISM can
                                                         # be slow.
  $arff_timeout = 60*5  unless defined $arff_timeout;
  $dtree_timeout = 60*3  unless defined $dtree_timeout;
  $plot_timeout = 60*3  unless defined $plot_timeout;

  $description = "Run a PRISM sim, and digest its output into ARFF files etc."
      unless defined $description;
  $keywords = [ qw / prism sim arff / ]  unless defined $keywords;
  $defaultIgnore = 0  unless defined $defaultIgnore;

  my $samples_or_method;
  if ($sample_size == 0) {
    $samples_or_method = '-simmethod';
    $sample_size = 'sprt';
  } else {
    $samples_or_method = '-simsamples';
  }

  my $test = {
    description => $description,
    keywords => $keywords,
    defaultIgnore => $defaultIgnore,

    agents => [
      {
        name         => "prism-pos",
        description  => "Run PRISM in -sim mode for positive examples",
        executable   => "prism",
        timeout      => $prism_timeout,
        add_args     => [ # The model --- always the first argument.
                          $prism_model,
                          '-pctl',

                          # The property to examine in the sim:
                          $prism_property,
                          '-sim', '-simoutputtruepaths',

                          # Number of simulations, or use SPRT.
                          $samples_or_method, $sample_size,

                          # Seed the randomizer for reproduceable
                          # results.
                          '-simseed', $sample_seed,

                          ## Don't need to give this for time-limited
                          ## properties.
                          # Maximum allowed path length.
                          # '-simpathlen', $sample_pathlen,
            ],
        exit_code    => 0,
        comparo      => undef, # This is going to be a huge file, which
                               # if incorrect will make the subsequent
                               # artifacts incorrect as well.
                               # Therefore, we won't waste time with a
                               # comparo for it --- we'll catch errors
                               # in the much smaller ARFF files.
        complete_before_continuing => 1,
      },

      {
        name         => "prism-neg",
        description  => "Run PRISM -cex mode for analytic counterexamples",
        executable   => "prism",
        timeout      => $prism_timeout,
        add_args     => [# The model --- always the first argument:
                         $prism_model,

                         # The property to examine in the sim:
                          '-pctl', $prism_property,

                         # Run in analytic counterexample mode:
                         '-s', '-cex', 'default'
            ],
        exit_code    => 0,
        comparo      => undef, # This is going to be a huge file, which
                               # if incorrect will make the subsequent
                               # artifacts incorrect as well.
                               # Therefore, we won't waste time with a
                               # comparo for it --- we'll catch errors
                               # in the much smaller ARFF files.
        complete_before_continuing => 1,
      },

      {
        name         => "prism",
        description  => "Consolidate previous two log files into one",
        command      => sub {
          my $agent_spec = shift;
          return "cat",
            $agent_spec->testspec->results_dir.'/prism-pos.log',
            $agent_spec->testspec->results_dir.'/prism-neg.log';
        },
        timeout      => $prism_timeout,
        exit_code    => 0,
        comparo      => undef, # This is going to be a huge file, which
                               # if incorrect will make the subsequent
                               # artifacts incorrect as well.
                               # Therefore, we won't waste time with a
                               # comparo for it --- we'll catch errors
                               # in the much smaller ARFF files.
        complete_before_continuing => 1,
      },

      {
        name         => "mval-nom-arff",
        logfile      => "mval-nom.arff", # Because waffles will object
                                         # to the .log file extension.
        description  => "Build multivalued-nominal style ARFF",
        command      => sub {
          my $agent_spec = shift;
          return "$culprits_dir/src/prism-sim-proc",
          '-s', 'parc-arff-multival-nominal',
          '-i', $agent_spec->testspec->results_dir.'/prism.log';
        },
        timeout      => $arff_timeout,
        exit_code    => 0,
        complete_before_continuing => 1,
        comparo      => new PRT::FullArffComparo(),
      },

      {
        name         => "asym-bin-arff",
        logfile      => "asym-bin.arff", # Because waffles will object
                                         # to the .log file extension.
        description  => "Build asymmetric binary style ARFF",
        command      => sub {
          my $agent_spec = shift;
          return "$culprits_dir/src/prism-sim-proc",
          '-s', 'parc-arff-asymmetric-binary',
          '-i', $agent_spec->testspec->results_dir.'/prism.log';
        },
        timeout      => $arff_timeout,
        exit_code    => 0,
        complete_before_continuing => 1,
        comparo      => new PRT::FullArffComparo(),
      },

      {
        name         => "mval-nom-json-dtree",
        logfile      => "mval-nom-dtree.json",
        description  => "Build a JSON model of the decision tree constructed by waffles from the multivalued-nominal style ARFF",
        command      => sub {
          my $agent_spec = shift;
          return "$waffles_dir/bin/waffles_learn",
          "train", $agent_spec->testspec->results_dir.'/mval-nom.arff', "decisiontree",
          "-leafthresh", $waffles_leafthresh,
          "-maxlevels", $waffles_maxlevels;
        },
        timeout      => $dtree_timeout,
        exit_code    => 0,
        complete_before_continuing => 1,
        comparo      => undef,
      },

      {
        name         => "asym-bin-json-dtree",
        logfile      => "asym-bin-dtree.json",
        description  => "Build a JSON model of the decision tree constructed by waffles from the asymmetric-binary style ARFF",
        command      => sub {
          my $agent_spec = shift;
          return "$waffles_dir/bin/waffles_learn",
          "train", $agent_spec->testspec->results_dir.'/asym-bin.arff', "decisiontree",
          "-leafthresh", $waffles_leafthresh,
          "-maxlevels", $waffles_maxlevels;
        },
        timeout      => $dtree_timeout,
        exit_code    => 0,
        complete_before_continuing => 1,
        comparo      => undef,
      },

      {
        name         => "mval-nom-json-dtree-plot",
        logfile      => "mval-nom-dtree.txt",
        description  => "Plot the JSON model of the decision tree constructed by waffles from the multivalued-nominal style ARFF",
        command      => sub {
          my $agent_spec = shift;
          return "$waffles_dir/bin/waffles_plot", "printculprits",
          $agent_spec->testspec->results_dir.'/mval-nom-dtree.json',
          $agent_spec->testspec->results_dir.'/mval-nom.arff';
        },
        timeout      => $plot_timeout,
        exit_code    => 0,
        complete_before_continuing => 0,
        comparo      => new PRT::LineComparo(),
      },

      {
        name         => "asym-bin-json-dtree-plot",
        logfile      => "asym-bin-dtree.txt",
        description  => "Plot the JSON model of the decision tree constructed by waffles from the multivalued-nominal style ARFF",
        command      => sub {
          my $agent_spec = shift;
          return "$waffles_dir/bin/waffles_plot", "printculprits",
          $agent_spec->testspec->results_dir.'/asym-bin-dtree.json',
          $agent_spec->testspec->results_dir.'/asym-bin.arff';
        },
        timeout      => $plot_timeout,
        exit_code    => 0,
        complete_before_continuing => 0,
        comparo      => new PRT::LineComparo(),
      } ]
  };

  if (defined $simpathv) {
    # The -simpathv option isn't the default for now --- need a bit
    # more to get it working.
    push @{$test->{agents}[0]{add_args}}, '-simpathv', $simpathv, 'stdout';
  } else {
    # Verbose mode gives us the output we need, but it's just kind of
    # temporary..
    push @{$test->{agents}[0]{add_args}}, '-v';
  }

  # Always end with a reference to the test specification hash reference.
  return $test;
}

1;
