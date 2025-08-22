#!/bin/bash

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
#CURL="curl --location --silent --user 00000000-0000-0000-0000-000000000000:secret"
CURL="curl --location --silent"
CAPI="${AIXCC_API_HOSTNAME}"
echo "Testing VDS Submission..."
echo "Submitting VDS to CAPI via service name..."

# ./submit-pov.sh <cp_name> <commit_sha1> <sanitizer_id> <harness_id> <base64_blob> <vc_id>

CP_NAME=$1
COMMIT=$2
SANITIZER_ID=$3
HARNESS_ID=$4

BLOB_FILE=$5
BLOB=$(base64 -w 0 ${BLOB_FILE})
# BLOB=$BLOB_FILE

VC_ID=$6

LAX_SHARED_DIR=$7

TIMESTAMP=$(date +"%Y%m%d-%H%M%S-%3N")
SUBMISSION_DIR="${LAX_SHARED_DIR}/submit-pov-jsons/${TIMESTAMP}"
mkdir -p ${SUBMISSION_DIR}

POV_DATA=$(printf "{\"cp_name\": \"%s\", \"pou\": {\"commit_sha1\": \"%s\", \"sanitizer\": \"%s\"}, \"pov\": {\"harness\": \"%s\", \"data\": \"%s\"}}" "$CP_NAME" "$COMMIT" "$SANITIZER_ID" "$HARNESS_ID" "$BLOB")

$CURL -X POST -H "Content-Type: application/json" ${CAPI}/submission/vds/ -d  "${POV_DATA}" > ${SUBMISSION_DIR}/vds-out.json

echo "CURL-ED"
jq < ${SUBMISSION_DIR}/vds-out.json
# echo ""
# sleep 1

VDS_UUID=$(jq < ${SUBMISSION_DIR}/vds-out.json -r '.vd_uuid')
VDS_STATUS=$(jq < ${SUBMISSION_DIR}/vds-out.json -r '.status')

# FIXME This really needs a timeout. capi sometimes never changes from pending?
while [ "$VDS_STATUS" == "pending" ]; do
	echo "VDS status is pending. Waiting..."
	sleep 5  # CAN CHANGE
	$CURL "${CAPI}/submission/vds/${VDS_UUID}" > ${SUBMISSION_DIR}/vds-status.json
	jq < ${SUBMISSION_DIR}/vds-status.json
	VDS_STATUS=$(jq < ${SUBMISSION_DIR}/vds-status.json -r '.status')
done

if [ "$VDS_STATUS" == "rejected" ]; then
	echo "POV rejected: ${GP_UUID}"
	CPV_UUID=$(jq < ${SUBMISSION_DIR}/vds-status.json -r '.cpv_uuid') # LB - should be NULL
	$thisdir/tell-optimus-vd-submission-status $VDS_STATUS ${VDS_UUID} $CPV_UUID $VC_ID
	exit -1
fi

if [ "$VDS_STATUS" == "accepted" ]; then
	echo "POV accepted: ${GP_UUID}"
	$CURL "${CAPI}/submission/vds/${VDS_UUID}" > ${SUBMISSION_DIR}/cpv-uuid.json
	CPV_UUID=$(jq < ${SUBMISSION_DIR}/cpv-uuid.json -r '.cpv_uuid')
	echo "CPV_UUID: ${CPV_UUID}"
	$thisdir/tell-optimus-vd-submission-status $VDS_STATUS ${VDS_UUID} $CPV_UUID $VC_ID

	TIME_STAMP=$(date '+%d%m-%H:%M')
	cp ${SUBMISSION_DIR}/cpv-uuid.json ${SUBMISSION_DIR}/${TIME_STAMP}-cpv-uuid.json
	
	exit 0
fi

# rm vds-out.json vds-status.json 2> /dev/null
