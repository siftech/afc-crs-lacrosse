#!/bin/bash
#  
# Initialization for final.  
# Run by optimus as soon as it receives new ctask.
# All that this script needs from optimus is the experiment dir.

MY_NAME=${AGENT_NAME}

thisdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd -P "$( dirname "$SOURCE" )" && /bin/pwd )"
PROGIMAGE=$(basename "$0")
warn()  { echo "$PROGIMAGE: ${@}" 1>&2; }
die()   { warn "${@}"; exit 1; }
dbug()   { test -z $DEBUG || warn "${@}"; }

printenv

# getopt requires -o (short opts) arg, empty here.
VALID_ARGS=$(getopt -o '' --long exp-dir:,task-id:  -- "$@")

if [[ $? -ne 0 ]]; then
    exit 1;
fi

eval set -- "$VALID_ARGS"
while [ : ]; do
    case "$1" in
        --exp-dir)
            echo "Processing 'exp-dir' option"
            EXP_DIR=$2
            shift 2
            ;;
        --task-id)
            echo "Processing 'task-id' option"
            TASK_ID=$2
            shift 2
            ;;
	--) shift;
            break
            ;;
    esac
done

CP_ROOT=${AIXCC_CP_ROOT:-/cp_root}
OUTPUT_DIR=${EXP_DIR}/${TASK_ID}/crs/shared
CP_NAME=`ls --indicator-style=none /cp_root`
CP_PATH=${CP_ROOT}/${CP_NAME}
MY_CP_PATH=${EXP_DIR}/${MY_NAME}/${CP_NAME}
PROJECT_YAML=${CP_PATH}/project.yaml

echo "EXP_DIR: ${EXP_DIR}"
#echo "CP_ROOT: ${CP_ROOT}"
echo "OUTPUT_DIR: ${OUTPUT_DIR}"
#echo "CP_NAME: ${CP_NAME}"
echo "CP_PATH: ${CP_PATH}"
echo "MY_CP_PATH: ${MY_CP_PATH}"
#echo "PROJECT_YAML: ${PROJECT_YAML}"

#if [ -v DEBUG ]; then
#    # buncha sanity checks
#    ls -al /;
#    ls -al ${CP_ROOT};
#    ls -al ${CP_PATH};
#    cat ${CP_PATH}/CHANGELOG.md;
#    ls -al /crs_scratch;
#
#    echo "Listing ${EXP_DIR}";
#    ls -al ${EXP_DIR};
#fi

#echo rsyncing corpus...
#mkdir -p /crs_scratch/canned-corpus-copy/
#rsync -av /lacrosse/code/corpus/ /crs_scratch/canned-corpus-copy/

# create shared scratch dir
mkdir -p ${OUTPUT_DIR}

echo "lax-final-init-optimus.sh complete"
touch ${OUTPUT_DIR}/lax-init

exit
