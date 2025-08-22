#!/bin/bash

submit-patch.sh CPV_UUID PATCHFILE VC_ID PC_ID

set -e
set -x

PROGNAME=$(basename "$0")
warn()  { echo "$PROGNAME: ${@}" 1>&2; }
die()   { warn "${@}"; exit 1; }
dbug()   { test -z $DEBUG || warn "${@}"; }

# bash-ism to get location of script.  Must use /bin/pwd to get absolute path.
thisdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd -P "$( dirname "$SOURCE" )" && /bin/pwd )"
dbug thisdir is $thisdir


# curl --user "id : secret" will be defined closer to competition time in sandbox/env
CURL="curl --location --silent"
CAPI="${AIXCC_API_HOSTNAME}"
echo "Testing GP Submission..."
echo "Submitting GP to CAPI via service name..."

# Feeding CPV_UUID and .patch file path as args 1 and 2 respectively.
CPV_UUID=$1
PATCHFILE=$2
VC_ID=$3
PC_ID=$4
LAX_SHARED_DIR=$5

#PATCHFILE="mockcp-test-patch.patch"
DATA=$(base64 -w 0 ${PATCHFILE})

TIMESTAMP=$(date +"%Y%m%d-%H%M%S-%3N")
SUBMISSION_DIR="${LAX_SHARED_DIR}/submit-patch-jsons/${TIMESTAMP}"
mkdir -p ${SUBMISSION_DIR}

$CURL -X POST -H "Content-Type: application/json" ${CAPI}/submission/gp/ -d "{\"cpv_uuid\": \"${CPV_UUID}\", \"data\": \"${DATA}\"}"  > ${SUBMISSION_DIR}/gp-out.json

jq < ${SUBMISSION_DIR}/gp-out.json

# echo ""
# sleep 1

GP_UUID=$(jq < ${SUBMISSION_DIR}/gp-out.json -r '.gp_uuid')
GP_STATUS=$(jq < ${SUBMISSION_DIR}/gp-out.json -r '.status')

# FIXME This really needs a timeout. capi sometimes never changes from pending?
while [ "$GP_STATUS" == "pending" ]; do
	echo "GP status is pending. Waiting..."
	sleep 5
	$CURL "${CAPI}/submission/gp/${GP_UUID}" > ${SUBMISSION_DIR}/gp-status.json
	jq < ${SUBMISSION_DIR}/gp-status.json
	GP_STATUS=$(jq < ${SUBMISSION_DIR}/gp-status.json -r '.status')
done

# FIXME send this to optimus, too!  
if [ "$GP_STATUS" == "rejected" ]; then
	echo "Patch rejected: ${GP_UUID}"
	$thisdir/tell-optimus-gp-submission-status ${GP_STATUS} ${CPV_UUID} ${GP_UUID} ${VC_ID} ${PC_ID}
	exit -1
fi

if [ "$GP_STATUS" == "accepted" ]; then
	echo "Patch accepted: ${GP_UUID}"
    $thisdir/tell-optimus-gp-submission-status ${GP_STATUS} ${CPV_UUID} ${GP_UUID} ${VC_ID} ${PC_ID}
	exit 0
fi

#rm gp-status.json 2> /dev/null
