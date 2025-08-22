#!/bin/bash
# pull out filenames of a git commit (to stdout)

set -x

commit=$1
outdir=$2
SOURCE_DIR=$3

#outdir=$( realpath `ls -d -1 -t out/output/*run_pov |head -1` )
#echo outdir=$outdir
outfile=$outdir/stdout.log
#echo outfile=$outfile
errfile=$outdir/stderr.log
#echo errfile=$errfile
patch=$outdir/patch
#echo patch=$patch

#SOURCE_DIR=$(yq '.cp_sources | keys | .[0]' project.yaml)
SRC_FOLDER="src/${SOURCE_DIR}"
#echo $SRC_FOLDER
cd $SRC_FOLDER
#/bin/pwd

# "The r1^! notation includes commit r1 but excludes all of its parents. " (gitrevision(7))
git diff "${commit}^!" > $patch
#echo patch is:
#cat $patch

#files=`lsdiff $patch |xargs /usr/bin/realpath -a | tr '\n' ' '`	# all files mentioned in patch, on one line
#files=`lsdiff $patch |xargs /usr/bin/basename -a | tr '\n' ' '`	# all files mentioned in patch, on one line
files=`lsdiff --strip 1 -E $patch | tr '\n' ' '`	# all files mentioned in patch, on one line
echo -n $files	# no newline, that messes up lisp msg passing
#for f in $files 
### FIXME this only works if sanitizer puts its file names etc in stderr
#do
#	if ( grep -q $f $errfile ); then
#  		filterdiff -i "*/$f" $patch
#	fi
#done

