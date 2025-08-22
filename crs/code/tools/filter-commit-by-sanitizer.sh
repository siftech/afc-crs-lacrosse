#!/bin/bash
# pull out only chunks of a git commit that are applying to files mentioned in a run_pov sanitizer report in second arg
# - redirect stdout to new minimized patch file.

set -x

commit=$1
outdir=$2
#san=$2

#outdir=$( realpath `ls -d -1 -t out/output/*run_pov |head -1` )
#echo outdir=$outdir
outfile=$outdir/stdout.log
#echo outfile=$outfile
errfile=$outdir/stderr.log
#echo errfile=$errfile
patch=$outdir/patch
#echo patch=$patch

SOURCE_DIR=$(yq '.cp_sources | keys | .[0]' project.yaml)
SRC_FOLDER="src/${SOURCE_DIR}"
cd $SRC_FOLDER
#/bin/pwd

git diff $commit > $patch
#echo patch is:
#cat $patch

files=`lsdiff $patch |xargs /usr/bin/basename -a`	# all files mentioned in patch
for f in $files 
## FIXME this only works if sanitizer puts its file names etc in stderr
do
	if ( grep -q $f $errfile ); then
  		filterdiff -i "*/$f" $patch
	fi
done

