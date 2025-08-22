#!/bin/bash

# build-and-test-pov.sh pov harness cp opt_no_build

# this just runs the standard run.sh build and checks if given pov still fails the CP via harness; for use with git bisect.
# Pass in the CP dir, so this can run itself there b/c it needs to use run.sh
# If the CP fails, returns 1, else 0.

set -x
# NOTE do not set -e here, we have proper error handling!

thisdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd -P "$( dirname "$SOURCE" )" && /bin/pwd )"

pov=$1
harness=$2
cp=$3
opt_no_build_arg=$4
patch=$5

# don't build if opt_no_build_arg is 0 or omitted.
if [[ -z $opt_no_build_arg || $opt_no_build_arg == 0 ]]; then
    no_build=0;
else
    no_build=1;
fi

cd $cp
echo pwd: 
/bin/pwd

if [[ $no_build == 0 ]]; then
    ./run.sh -x build $patch
    if [ $? -ne 0 ]; then exit 125; fi # special exitcode for bisect if a version fails build/test
fi

./run.sh -x run_pov $pov $harness
if [ $? -ne 0 ]; then exit 125; fi	

# NOTE run.sh doesn't handle the harness detecting failure, so
# the stuff below checks for any sanitizer pattern in the most recent cmd output
# This *could* only look for a specific designated sanitizer, but presumably we
# dont need to do that... 

outdir=`ls -d -1 -t out/output/*run_pov |head -1`
outfile=$outdir/stdout.log
errfile=$outdir/stderr.log

yq '.sanitizers |.[]' project.yaml > $outdir/patterns.txt

if grep -F -f $outdir/patterns.txt $outfile; then
        echo "Found a sanitizer pattern in $outfile!"
        exit 1
fi

if grep -F -f $outdir/patterns.txt $errfile; then
        echo "Found a sanitizer pattern in $errfile!"
        exit 1
fi
exit 0  # didnt find string
