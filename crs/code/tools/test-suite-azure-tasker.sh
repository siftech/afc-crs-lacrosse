#!/usr/bin/env bash
# This will execute a test suite against our azure CRS using the AIxCC provided azure tasker.
# The test suite shall contain a task of each kind in the following order.
# Full task -> Sarif -> Unharnessed task -> Delta task.
set -e

PROGNAME=$(basename "$0")
warn()  { echo "$PROGNAME: ${@}" 1>&2; }
die()   { warn "${@}"; exit 1; }
dbug()   { test -z $DEBUG || warn "${@}"; }

thisdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd -P "$( dirname "$SOURCE" )" && /bin/pwd )"
testdir="${thisdir}/../test"
dbug "thisdir is $thisdir"
dbug "testdir is $testdir"

# Path to the lax neverending PRT Test.
PATH_TO_NEVERENDING=$1
dbug "Path to neverending is $PATH_TO_NEVERENDING"
if [ ! -d "$PATH_TO_NEVERENDING" ]; then
  die "Path to neverending does not exist or was not supplied."
fi

if [ -z "${COMPETITION_API_TEAM_ID+x}" ]; then
    die "COMPETITION_API_TEAM_ID undefined"
fi

if [ -z "${COMPETITION_API_TEAM_SECRET+x}" ]; then
    die "COMPETITION_API_TEAM_SECRET undefined"
fi

# Send a libpng ful task.
warn "Triggering libpng full task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "git@github.com:aixcc-finals/example-libpng.git",
    "challenge_repo_head_ref": "fdacd5a1dcff42175117d674b0fda9f8a005ae88",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "v1.0.0",
    "fuzz_tooling_project_name": "libpng",
    "duration": 3600
}'

# Sleep for 3 minutes. Let our CRS API receive the full task. Can take anywhere from 15 seconds to 2.5 minutes.
warn "Sleeping for 3 minutes before issuing sarif."
sleep 3m;

# Look for the task_id of the full task in the CRS API Logs.
API_LOG="${PATH_TO_NEVERENDING}/start-afc-lax-api-server.log"
if [ ! -f "$API_LOG" ]; then
  die "OPTI Log file ${API_LOG} not found for triggering a sarif."
fi

# Get the TASK_ID of the full scan from the API_LOGS
dbug "Looking in $API_LOG for TASK_ID"
TASK_ID=$(grep -oP '\(:TASK_ID "\K[^"]+(?="\))' "$API_LOG" | tail -n 1)
dbug "TASK_ID is $TASK_ID"

# Build payload using task ID and sarif data
SARIF_FILE="${testdir}/libpng-full-sarif.json"
SARIF_DATA=$(cat "$SARIF_FILE")
dbug "sarif data is $SARIF_DATA"
SARIF_PAYLOAD=$(jq -n --arg task_id "$TASK_ID" --argjson sarif "$SARIF_DATA" '{task_id: $task_id, sarif: $sarif}')

# Use the TASK_ID in the trigger sarif curl command.
warn "Triggering sarif command"
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/sarif' -H 'Content-Type: application/json' -d "$SARIF_PAYLOAD"

# Sleep for 3 minutes. Let our CRS API receive the sarif. Should take < 3 minutes.
warn "Sleeping for 3 minutes before issuing unharnessed ctask."
sleep 3m;

# Send an unharnessed libxml2 full task
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "git@github.com:aixcc-finals/afc-libxml2.git",
    "challenge_repo_head_ref": "challenges/lx-full-01",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/lx-full-01",
    "fuzz_tooling_project_name": "libxml2",
    "duration": 36000,
    "harnesses_included":false
}'

# Sleep for 3 minutes. Let our CRS API receive the libxml2 full task, should take < 3 minutes.
warn "Sleeping for 3 minutes before issuing freerdp delta with 5 minute a deadline"
sleep 3m;

# Send a freerdp delta task
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "git@github.com:aixcc-finals/afc-freerdp.git",
    "challenge_repo_base_ref": "a92cc0f3ebc3d3f4cf5b6097920a391e9b5fcfcf",
    "challenge_repo_head_ref": "challenges/fp-delta-01",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/fp-delta-01",
    "fuzz_tooling_project_name": "freerdp",
    "duration": 300
}'

# Sleep for 3 minutes. Let our CRS API receive the libxml2 full task, should take < 3 minutes.
warn "Sleeping for 3 minutes before issuing zookeeper delta 01."
sleep 3m;

# Send a freerdp delta task
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
        "challenge_repo_url": "https://github.com/aixcc-finals/afc-zookeeper.git",
        "challenge_repo_base_ref": "d19cef9ca254a4c1461490ed8b82ffccfa57461d",
        "challenge_repo_head_ref": "5ee4f185d0431cc88f365ce779aa04a87fe7690f",
        "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
        "fuzz_tooling_ref": "challenge-state/zk-ex1-delta-01",
        "fuzz_tooling_project_name": "zookeeper",
        "duration": 3600
}'
