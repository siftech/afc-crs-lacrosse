#! /bin/bash

base_dir="/home/angr/angr-dev"

pwd
ls $base_dir

# Roll back all angr stuff to a date which is known to work
for dir in $base_dir/*/
do
    cd $dir
    # Roll back a directory if it is a python lib or vex.
    # 20200125 MAD - I don't recall if there is any reason why we don't just roll back
    # all angr-dev directories.
    # 20230131 SJ - There is now a reason. Roll back all dir except wheels/
    if { [ -a ".git" ] || [ "$dir" == "$base_dir/vex/" ]; } && [ "$dir" != "$base_dir/wheels/" ]; then
	rev=$(git rev-list -n 1 --first-parent --before='2019-02-04 02:00' master || true)
	if ! [ -z $rev ] ; then
	    echo "Rolling back $PWD"
	    git checkout $rev
	    git log | head
	fi
    fi	
done

# Roll forward setup.py of angr-targets
cp /angr-targets-setup.py $base_dir/angr-targets/setup.py

echo "Finished rolling back repos"
