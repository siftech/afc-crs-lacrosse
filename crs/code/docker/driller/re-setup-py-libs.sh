#! /bin/bash

# Re-install the python libraries that already exist in the container,
# but which we are editing. This is required so that new packages are
# found and new dependencies are installed.
# Needs to run when the container is first started.

. /home/angr/.virtualenvs/angr/bin/activate
base_dir="/home/angr/angr-dev"

for dir in "tracer" "driller"
do
    full_path=$base_dir/$dir
    echo "Setting up in " $full_path
    (cd $full_path; pip install -e .)
done
