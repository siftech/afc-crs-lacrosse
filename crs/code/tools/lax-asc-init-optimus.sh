#!/bin/bash
#  
# Update initialization for asc.  Now expecting to run be run by
# optimus as soon as it receives :new-challenge-project msg.
# All that this script needs from optimus is the experiment dir.

MY_NAME=${AGENT_NAME}

thisdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd -P "$( dirname "$SOURCE" )" && /bin/pwd )"
PROGIMAGE=$(basename "$0")
warn()  { echo "$PROGIMAGE: ${@}" 1>&2; }
die()   { warn "${@}"; exit 1; }
dbug()   { test -z $DEBUG || warn "${@}"; }

printenv

# getopt requires -o (short opts) arg, empty here.
VALID_ARGS=$(getopt -o '' --long exp-dir: -- "$@")

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
	--) shift;
            break
            ;;
    esac
done

CP_ROOT=${AIXCC_CP_ROOT:-/cp_root}
OUTPUT_DIR=${EXP_DIR}/crs/shared
CP_NAME=`ls --indicator-style=none /cp_root`
CP_PATH=${CP_ROOT}/${CP_NAME}
MY_CP_PATH=${EXP_DIR}/${MY_NAME}/${CP_NAME}
PROJECT_YAML=${CP_PATH}/project.yaml

echo "EXP_DIR: ${EXP_DIR}"
#echo "CP_ROOT: ${CP_ROOT}"
#echo "OUTPUT_DIR: ${OUTPUT_DIR}"
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

echo rsyncing corpus...
mkdir -p /crs_scratch/canned-corpus-copy/
rsync -av /lacrosse/code/corpus/ /crs_scratch/canned-corpus-copy/

# translate project.yaml to project.json for convenience of other tools
mkdir -p ${OUTPUT_DIR}
yq -p yaml -o json ${PROJECT_YAML} > ${OUTPUT_DIR}/project.json

CP_SOURCES=`yq -r '.cp_sources | keys | .[]' ${PROJECT_YAML}`
echo "CP_SOURCES: ${CP_SOURCES}"
echo

#   "Every bash script longer than six lines should've been a perl (python?) script." - Socrates
for cp_src in ${CP_SOURCES}; do
    echo "Git groveling for ${cp_src}"
    cp_src_dir=${MY_CP_PATH}/src/${cp_src}
    if [ -v DEBUG ]; then
	echo "Listing cp_src_dir:";
	ls -al ${cp_src_dir};
	echo
    fi
    git_out_dir=${OUTPUT_DIR}/git-info/${cp_src}
    mkdir -p ${git_out_dir}
    cd ${cp_src_dir}
    
    # FIXME temp debug  
    echo "pwd: $PWD"
    REV0=`git rev-list --max-parents=0 HEAD`
    git log --format=fuller --stat ${REV0}..HEAD | jc --git-log  -p > ${git_out_dir}/git-log.json

    mkdir -p ${git_out_dir}/git-commits

    # FIXME temp debug  
    #echo "sha list: $(git rev-list --min-parents=1 HEAD)"
    #echo
    for SHA in $(git rev-list --min-parents=1 HEAD);
    do
	#echo "patch list: "
	#git show --patch $SHA
	git show --patch $SHA > ${git_out_dir}/git-commits/$SHA;
	echo
    done
    # FIXME temp debug  
    echo "ls git-commits:"
    #ls -al ${git_out_dir}/git-commits
    echo
done
echo "lax-asc-init-optimus.sh complete"
touch ${OUTPUT_DIR}/lax-init

exit
