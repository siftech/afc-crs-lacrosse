#!/bin/bash

# getopt requires -o (short opts) arg, empty here.
VALID_ARGS=$(getopt -o '' --long target-dir:,cp-address:,repo-name:,output-dir: -- "$@")

if [[ $? -ne 0 ]]; then
    exit 1;
fi

eval set -- "$VALID_ARGS"
while [ : ]; do
    case "$1" in
        --target-dir)
            #echo "Processing 'target-dir' option"
            TARGET_DIR=$2
            shift 2
            ;;
        --cp-address)
            #echo "Processing 'cp-address' option"
            CP_ADDRESS=$2
            shift 2
            ;;
        --repo-name)
            #echo "Processing 'repo-name' option"
            REPO_NAME=$2
            shift 2
            ;;
        --output-dir)
            #echo "Processing 'output-dir' option"
            OUTPUT_DIR=$2
            shift 2
            ;;
        --) shift;
            break
            ;;
    esac
done

echo "TARGET_DIR: ${TARGET_DIR}"
echo "CP_ADDRESS: ${CP_ADDRESS}"
echo "REPO_NAME: ${REPO_NAME}"
echo "OUTPUT_DIR: ${OUTPUT_DIR}"

ls -al /

ls -al /cp_root
ls -al /cp_root/mock-cp
cat /cp_root/mock-cp/CHANGELOG.md
ls -al /crs_scratch


ls -al ${TARGET_DIR}

# clone cp
#cd ${TARGET_DIR}
#git clone ${CP_ADDRESS}

# get source
#cd ${REPO_NAME}
#if [ ${REPO_NAME} = "aixcc-cp-001-thin" ]; then
    #   special case so we can use this test a bit longer
    # (yes, this is already deprecated as I write it  ,
    # recent cp-sandbox updates indicate this will be something like "make cpsrc-prepare")
#    ./run.sh pull_source
#else
#    make cpsrc-prepare
#fi

# translate project.yaml to project.json for convenience of other tools
yq -p yaml -o json project.yaml > ${OUTPUT_DIR}/project.json


# More out-of-date code.  I assume updated cps will have a bit more complex
# source dir structure.  

# get SHA for initial commit.
# FIXME use yq magic to get name of directories
cd src
REV0=`git rev-list --max-parents=0 HEAD`

jc --version

# write json version of git log to out/git-log.json
git log --format=fuller --stat ${REV0}..HEAD | jc --git-log  -p > ${OUTPUT_DIR}/git-log.json

# create git-commits directory
mkdir ${OUTPUT_DIR}/git-commits
for SHA in $(git rev-list --min-parents=1 HEAD);
do
    git show --patch $SHA > ${OUTPUT_DIR}/git-commits/$SHA;
done

# get the required image.  FIXME get image name from project.yaml
echo "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" | docker login ghcr.io -u auth0-66083623154c2ff59d880957_aixcc --password-stdin
docker pull ghcr.io/aixcc-sc/mock-cp:v2.0.3
docker images

## FIXME this should probably be broken out as a separate agent task, since it could take a while
# cd ..
# ./run.sh build

