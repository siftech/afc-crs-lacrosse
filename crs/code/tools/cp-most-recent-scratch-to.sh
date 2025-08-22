#!/usr/bin/env bash

echo "Ere I am, JH"

thisdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd -P "$( dirname "$SOURCE" )" && /bin/pwd )"
PROGIMAGE=$(basename "$0")
warn()  { echo "$PROGIMAGE: ${@}" 1>&2; }
die()   { warn "${@}"; exit 1; }
dbug()   { test -z $DEBUG || warn "${@}"; }

# FIXME   Remove
DEBUG=1

# cp-most-recent-scratch-to.sh <dest>

# cp from most recent scratch directory to <target> directory.
# (When called by prt, <target> is likely to be the current results (ie, log) directory.)
DEST=$1
dbug "DEST: ${DEST}"

#   TODO Should share w cp-to-lax-refs.sh, prob via a common sub-script.

# Guess experiment path is the most recently created dir in crs_scratch.
EXP_PATH=$(ls -td $thisdir/../../../crs_scratch/* | head -n 1)
dbug "EXP_PATH: ${EXP_PATH}"
#EXP_PATH=$(realpath $thisdir/../../../crs_scratch/${EXP_DIR})
#dbug "REAL_PATH: ${REAL_PATH}"

SCRATCH_BITS_PATH="${DEST}/crs-scratch-bits/"
dbug "Creating ${SCRATCH_BITS_PATH}"
mkdir -p ${SCRATCH_BITS_PATH}

# copy patches
dbug "Collecting patches"
$thisdir/collect-patches.sh ${EXP_PATH} ${SCRATCH_BITS_PATH}/patches/

# copy crs dir (eg, vuln-cands and patch-cands) and output dirs from scratch.

#busted# # exclude executables (eg, harnesses)
#busted# # (-printf '%P\n' prints the relative paths w/o leading dot-slash)
#busted# TMP_EXCLUDE=$(mktemp)
#busted# (cd ${EXP_PATH}; \
#busted#  #find . -type f -executable -print > "${TMP_EXCLUDE}" )
#busted#  find . -type f -executable -printf '%P\n' > "${TMP_EXCLUDE}" )
#busted# if [ -v DEBUG ]; then
#busted#     echo "Excluding:"
#busted#     cat "${TMP_EXCLUDE}"
#busted# fi
#busted# 
#busted# # need to actually cd for globbing
#busted# # --exclude='crs/shared/fuzz' excludes fuzzer seeds
#busted# # --exclude-from="${TMP_EXCLUDE}" excludes executables, eg, harnesses (see above)
#busted# (cd ${EXP_PATH}; \
#busted#  tar zcf "${SCRATCH_BITS_PATH}/scratch.tgz" \
#busted#      --exclude='crs/shared/fuzz' \
#busted#      --exclude-from="${TMP_EXCLUDE}" \
#busted#      crs \
#busted#      FUZZBOMB*/out \
#busted#      FUZZBOMB*/*/work \
#busted#      FUZZBOMB*/*/out
#busted#  )
#busted# 
rm "${TMP_EXCLUDE}"
