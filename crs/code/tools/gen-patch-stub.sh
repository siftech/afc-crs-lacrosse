#!/bin/bash

echo "on our way"

#   hacked stub while waiting patch research
# expect one and only one arg -- the absolute path to the out dir of the target

out_path=$1

target_path="$(dirname "${out_path}")"

if [ -z "$2" ]
  then
    target_dirname="$(basename ${target_path})"
else
    target_dirname=$2
fi

# bash-ism to get location of script.
thisdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd -P "$( dirname "$SOURCE" )" && /bin/pwd )"

echo ${out_path}
echo ${target_path}
echo ${target_dirname}

if [ -d "$thisdir/stub-patches/${target_dirname}" ]; then
    echo "TASK RESULT: copying stub patch."
    cp $thisdir/stub-patches/${target_dirname}/* ${out_path}
fi

echo "GEN-PATCH-STUB - TASK RESULT: DONE"
