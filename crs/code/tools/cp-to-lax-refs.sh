#!/usr/bin/env bash

# Run in log (results) dir to create two tar files and a summ in /tmp/lax-refs on this host.

thisdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd -P "$( dirname "$SOURCE" )" && /bin/pwd )"

RUN_NAME=$(basename $(pwd))
echo RUN_NAME: ${RUN_NAME}
RUN_DIR=$(dirname $(pwd))
echo RUN_DIR: ${RUN_DIR}

PARTIAL_EXP_DIR=$(grep -m 1 -oP "Creating \*experiment-dir\*: /lacrosse\K.*" OPTIMUS0.log)
EXP_PATH=$(realpath "${LACROSSE_HOME}/${PARTIAL_EXP_DIR}")
echo EXP_PATH: ${EXP_PATH}
EXP_NAME=$(basename ${EXP_PATH})
echo EXP_NAME: ${EXP_NAME}
EXP_DIR=$(dirname ${EXP_PATH})
echo EXP_DIR: ${EXP_DIR}

mkdir -p /tmp/lax-refs

# save the logs
tar zcf "/tmp/lax-refs/${RUN_NAME}-logs.tgz" -C ${RUN_DIR} ${RUN_NAME}

# copy patches
$thisdir/collect-patches.sh ${EXP_PATH} /tmp/lax-refs/${RUN_NAME}-patches/

# save all the scratch version
#tar zcf "/tmp/lax-refs/${RUN_NAME}-scratch.tgz" -C ${EXP_DIR} ${EXP_NAME}

# more selective version:
# - ${EXP_NAME}/crs -- shared (povs, bics, patches, etc)
# - ${EXP_NAME}/FUZZBOMB*/out -- task scratch dirs
# - ${EXP_NAME}/FUZZBOMB*/*/work -- fuzzer scratch dir
# - ${EXP_NAME}/FUZZBOMB*/*/out -- run.sh scratch dir

# need to actually cd for globbing
(cd ${EXP_DIR}; \
 tar zcf "/tmp/lax-refs/${RUN_NAME}-scratch.tgz" \
     ${EXP_NAME}/crs \
     ${EXP_NAME}/FUZZBOMB*/out \
     ${EXP_NAME}/FUZZBOMB*/*/work \
     ${EXP_NAME}/FUZZBOMB*/*/out
 )

summ > "/tmp/lax-refs/${RUN_NAME}-summ.txt"
