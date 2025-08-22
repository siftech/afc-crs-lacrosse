#!/bin/bash
# Run this in the CP dir and pass in harness name from project.yaml
# Need to know which harness you want to fuzz... then we treat it like a libfuzzer tgt wherein the
# tgt is linked w/ the fuzzer lib and it essentially fuzzes itself.
# We customize the sift_harness.sh to work on the right harness, using it to wrap harness, to avoid the
# run.sh infra sending in just one file...which allows libfuzzer to fuzz starting from a corpus directory.
# Otherwise, the run.sh infra will send in one file and no fuzzing shall happen.

# If you have seeds from an LLM or other source, place in work/seeds dir under CP dir

set -ex

harness=$1
echo harness is $harness
#seed_dir=$2	# optional dir of seeds, eg from LLM

# this is where this source script lives, not the cwd
thisdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd -P "$( dirname "$SOURCE" )" && /bin/pwd )"

## create empty seed_dir, in case none supplied?
mkdir -p work/seeds

#rewrite the sift_harness template with appropriate binary name(s)
cp $thisdir/../crs-sandbox-items/sift_harness.sh out
  # Note we use | as the separator below so that slashes in the $harness do not blow it up
sed -i -e "s|XXXHARNESSXXX|$harness|" out/sift_harness.sh

# if the cwd is the agent's CP copy, then ../crs/shared should be the shared area
# so we have the OPT init thing do the making of the dict
#$thisdir/make-string-dictionary ../crs/shared/strings.dict

touch fake # irrelevant but must-exist (empty) file for making run.sh run_pov (libfuzzer) happy
echo '"hello"' >> work/strings.dict # in case generator fails etc

FUZZER_CMD_OPTS="-jobs=10 -only_ascii=1" DOCKER_EXTRA_ARGS="-e FUZZER_CMD_OPTS" /lacrosse/code/prt/timestamp  ./run.sh -x -v run_pov fake sift_harness.sh |tee work/libfuzzer.log #2>&1
