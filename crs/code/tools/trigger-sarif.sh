#!/usr/bin/env bash
# Builds a curl to trigger a sarif from a respective full scan ctask

PROGNAME=$(basename "$0")
warn()  { echo "$PROGNAME: ${@}" 1>&2; }
die()   { warn "${@}"; exit 1; }
dbug()   { test -z $DEBUG || warn "${@}"; }
thisdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd -P "$( dirname "$SOURCE" )" && /bin/pwd )"

# Setup local example comp server endpoint vars
export CIRCA_BASEPORT=${CIRCA_BASEPORT:-10000}
dbug CIRCA_BASEPORT is $CIRCA_BASEPORT

export COMP_API_PORT=${COMP_API_PORT:-$(( CIRCA_BASEPORT + 501 ))}
LOCAL_EX_COMP_SERVER_ENDPOINT="http://localhost:${COMP_API_PORT}/webhook/sarif"

# Short sleep to let opti hand off the full scan task before receiving the sarif...
# warn "sleeping for 30 seconds"
# sleep 30
warn "sleeping for 2 minutes"
sleep 2m

# Gets an Opti log file then greps for the TASK_ID of the full scan.
API_LOG="$1"
if [ ! -f "$API_LOG" ]; then
  die "OPTI Log file ${API_LOG} not found for triggering a sarif."
fi

# Get sarif data to from defined file...
SARIF_FILE="$2"
if [ ! -f "$SARIF_FILE" ]; then
  die "SARIF Data file $SARIF_FILE not found."
fi

# Get the TASK_ID of the full scan from the API_LOGS
dbug "Looking in $API_LOG for TASK_ID"
TASK_ID=$(grep -oP '\(:TASK_ID "\K[^"]+(?="\))' "$API_LOG" | tail -n 1)
dbug "TASK_ID is $TASK_ID"

# Build payload using task ID and sarif data
SARIF_DATA=$(cat "$SARIF_FILE")
dbug "sarif data is $SARIF_DATA"
SARIF_PAYLOAD=$(jq -n --arg task_id "$TASK_ID" --argjson sarif "$SARIF_DATA" '{task_id: $task_id, sarif: $sarif}')

dbug "Sarif payload is $SARIF_PAYLOAD"

# Use the TASK_ID in the trigger sarif curl command. FIXME - This sarif curl is specific to libpng.
warn "Triggering sarif command"

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d "$SARIF_PAYLOAD"
