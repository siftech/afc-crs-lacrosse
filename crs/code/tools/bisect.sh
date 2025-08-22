#!/bin/bash
# run this in the CP dir, with full path to pov and the harness name from project.yaml
# looking for a line like this: 
# 8e2a8e613fe5b6f03cb8e0c27180a468671f03a8 is the first bad commit
# (which is from the nginx CP)

set -x
# NOTE do not set -e here, the call to build-and-test-pov.sh is expected to fail!

thisdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd -P "$( dirname "$SOURCE" )" && /bin/pwd )"

pov=$1
harness=$2
cp=`/bin/pwd`

#SOURCE_DIR=$(yq '.cp_sources | keys | .[0]' project.yaml)
SOURCE_DIR=$3

SRC_FOLDER="src/${SOURCE_DIR}"

# Note for the error msgs below, it is important  they all start the same so we can have
# a simple pattern match for error detection, instead of adding in both the msgs here and the 
# pattern matching in the corresponding lisp agent task process-line

# run pov once to ensure that it really is a POV
# FIXME can we avoid the build sometimes/always?  The build can be long for some CPs,
# even if just built...they may not be proper makefiles... eg, nginx always restarts configure, blah blah
$thisdir/build-and-test-pov.sh $pov $harness $cp
ret=$?
echo return val $ret
if [ $ret -ne 1 ]; then 
  echo "ERROR - git bisect aborted because supposed PoV did not trigger a sanitizer"
  exit 1
fi

# capture the just-run pov output dir for later use in filtering sanitizer output against the BIC
# Lisp will grab this line and use the outdir, after bisect works
outdir=$( realpath `ls -d -1 -t out/output/*run_pov |head -1` )
echo POV proof output dir: $outdir

#set -e	# now, past the expected-fail above, we can set this, to fail if some bisect thing pukes
# No, on second thought, we want to be able to trap fail and print error msg that lisp will see

echo changing to the source dir: 
cd $SRC_FOLDER
/bin/pwd

#git reset --hard HEAD
git bisect start HEAD `git rev-list --max-parents=0 HEAD`
ret=$?
echo return val $ret
if [ $ret -ne 0 ]; then 
  echo "ERROR - git bisect start failed"
fi
git bisect run $thisdir/build-and-test-pov.sh $pov $harness $cp
ret=$?
echo return val $ret
if [ $ret -ne 0 ]; then 
  echo "ERROR - git bisect failed"
fi
git bisect reset
ret=$?
echo return val $ret
if [ $ret -ne 0 ]; then 
  echo "ERROR - git bisect reset failed"
  exit $ret
fi
exit 0
