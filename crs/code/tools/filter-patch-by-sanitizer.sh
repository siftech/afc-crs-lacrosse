#!/bin/bash
# pull out only chunks of a git commit that are applying to files mentioned in the sanitizer report... 
# we'll see if it comes in handy. At least for nginx, it does the right thing and all we'd have to do is unapply the hunk it pulls out
# - redirect stdout to new minimized patch file.
# - filterdiff will also happily remove extra git commit noise, hope that's ok :)

patch=$1
san=$2

files=`lsdiff $patch |xargs /usr/bin/basename -a`	# all files mentioned in patch
for f in $files 
do
	if ( grep -q $f $san ); then
  		filterdiff -i "*/$f" $patch
	fi
done

