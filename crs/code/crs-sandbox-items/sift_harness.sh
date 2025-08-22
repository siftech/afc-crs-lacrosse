#!/bin/bash
# we want to prevent the "only run one example" argument from getting passed through
## FIXME is pov_harness always the name to wrap?  no i think not...  maybe grab w/ yq
## or have lisp copy this over and run sed subst on it
## FIXME may be able to set FUZZER_CMD_OPTS  if they make run.sh pass it through...
##	: "${FUZZER_CMD_OPTS:=-runs=1 -timeout=10}"

set -eux
set -- "${@:1:$(($#-1))}"  # https://stackoverflow.com/questions/20398499/remove-last-argument-from-argument-list-of-shell-script-bash

echo args are "$@"
mkdir -p /work/id_1-corpus
#/out/pov_harness $@ /work/id_1-corpus
#/out/pov_harness "$@" /work/corpus2 /work/just-from-llm
# Note the starting slash..their binary harness entries do not include that
/XXXHARNESSXXX "$@" /work/id_1-corpus /work/seeds
