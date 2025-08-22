#!/bin/bash

# DEFAULT BEHAVIOR - no args
# just run this in the CP dir and it will do the rest-- use basename $PWD to find the CP
# name, (ie the name of the directory under "cp_root" that contains the cp)
# to look up what POVs to use. then copy them to 'out' and print the magic line for each one.

# WITH ARGS
# For every arg, find all of the files in the tree rooted at arg and cp them as above.

#To make a much simpler fake-fuzz approach that will follow the actual code/reasoning path of just about everything,
#we just need a fake fuzzer that puts crashes where they belong and prints the string (locating the crash file)
#that the fuzzer task expects to grab to initiate all its reasoning

#For libfuzzer that looks like
#Test unit written to ./crash-1243aa9dab7bd452506fdc4f001ce62c7e98ac73]

thisdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd -P "$( dirname "$SOURCE" )" && /bin/pwd )"
PROGNAME=$(basename "$0")
warn()  { echo "$PROGNAME: ${@}" 1>&2; }
die()   { warn "${@}"; exit 1; }
dbug()   { test -z $DEBUG || warn "${@}"; }

cpname=`basename $PWD`
dbug cpname is $cpname

if [ $# -eq 0 ]; then
    for f in `ls $thisdir/stub-blobs/${cpname}` ; do
	dbug "Copying pov blob $thisdir/stub-blobs/${cpname}/$f to out."
	cp $thisdir/stub-blobs/${cpname}/$f out
	# Lisp task matches on this output
	echo "Test unit written to ./$f"
    done
else
    for arg in "$@"; do
	dbug "find and copy for $arg"
	# need to iterate over files to send triggering output
	#find $thisdir/stub-blobs/$arg -type f -print0 | xargs -0 -I{} cp {} out
	for f in $(find $thisdir/stub-blobs/$arg -type f); do
	    cp $f out
	    # Lisp task matches on this output
	    echo "Test unit written to ./$(basename $f)"
	done
    done
fi
