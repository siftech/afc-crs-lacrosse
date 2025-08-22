#!/bin/bash

#To make a much simpler fake-thing approach that will follow the actual code/reasoning path of just about everything,
#we just need a fake bisector that prints the string 
#that the lacrosse-git-bisect-task expects to grab to initiate all its reasoning

#For bisect that looks like
#11dafa9a5babc127357d710ee090eb4c0c05154f is the first bad commit
# and we can just make empty files with those names

# just run this in the CP dir and it will do the rest-- use pwd to find CP
# name, to help look up what BICs to use. then print the magic line for each one.

thisdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd -P "$( dirname "$SOURCE" )" && /bin/pwd )"
PROGNAME=$(basename "$0")
warn()  { echo "$PROGNAME: ${@}" 1>&2; }
die()   { warn "${@}"; exit 1; }
dbug()   { test -z $DEBUG || warn "${@}"; }

cpname=`basename $PWD`
dbug cpname is $cpname


for f in `ls $thisdir/stub-bics/${cpname}` ; do
    echo "$f is the first bad commit"
done

