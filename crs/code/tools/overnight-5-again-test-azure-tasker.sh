#!/usr/bin/env bash
# This will execute a test suite against our azure CRS using the AIxCC provided azure tasker.
# Remake of overnight 3 with the use of 'test-pixel-phantom-api' tasker and not the go time 'api'
# This will trigger some round 2 and round 3 tasks including a sarif and an unharnessed challenge.

set -e

PROGNAME=$(basename "$0")
warn()  { echo "$PROGNAME: $*" 1>&2; }
die()   { warn "${@}"; exit 1; }
dbug()   { test -z "$DEBUG" || warn "${@}"; }

thisdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd -P "$( dirname "$SOURCE" )" && /bin/pwd )"
testdir="${thisdir}/../test"
dbug "thisdir is $thisdir"
dbug "testdir is $testdir"

if [ -z "${COMPETITION_API_TEAM_ID+x}" ]; then
    die "COMPETITION_API_TEAM_ID undefined"
fi

if [ -z "${COMPETITION_API_TEAM_SECRET+x}" ]; then
    die "COMPETITION_API_TEAM_SECRET undefined"
fi

# Path to the lax neverending PRT Test.
PATH_TO_NEVERENDING=$1
dbug "Path to neverending is $PATH_TO_NEVERENDING"
if [ ! -d "$PATH_TO_NEVERENDING" ]; then
  die "Path to neverending does not exist or was not supplied."
fi

warn "Triggering libxml2 delta 02 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "git@github.com:aixcc-finals/afc-libxml2.git",
  "challenge_repo_base_ref": "0f876b983249cd3fb32b53d405f5985e83d8c3bd",
  "challenge_repo_head_ref": "challenges/lx-delta-02",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/lx-delta-02",
  "fuzz_tooling_project_name": "libxml2",
  "duration": 10800
}'

sleep 30s;

warn "Triggering integration test delta 01 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/integration-test.git",
  "challenge_repo_base_ref": "4a714359c60858e3821bd478dc846de1d04dc977",
  "challenge_repo_head_ref": "challenges/integration-test-delta-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/integration-test-delta-01",
  "fuzz_tooling_project_name": "integration-test",
  "duration": 10800
}'

sleep 30s;

warn "Triggering sqlite delta 01 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "https://github.com/aixcc-finals/afc-sqlite3.git",
    "challenge_repo_base_ref": "6a3e7f57f00f0a2b6b89b0db7990e3df47175372",
    "challenge_repo_head_ref": "challenges/sq-delta-01",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/sq-delta-01",
    "fuzz_tooling_project_name": "sqlite3",
    "duration": 10800
}'

# Sleep for 3 minutes. Let our CRS API receive the full task. Can take anywhere from 15 seconds to 2.5 minutes.
warn "Sleeping for 3 minutes before issuing sqlite delta 01 sarif."
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
SARIF_FILE="${testdir}/sqlite3-delta-01-sarif.json"
SARIF_DATA=$(cat "$SARIF_FILE")
dbug "sarif data is $SARIF_DATA"
SARIF_PAYLOAD=$(jq -n --arg task_id "$TASK_ID" --argjson sarif "$SARIF_DATA" '{task_id: $task_id, sarif: $sarif}')

# Use the TASK_ID in the trigger sarif curl command.
warn "Triggering tika delta 02 sarif."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/sarif' -H 'Content-Type: application/json' -d "$SARIF_PAYLOAD"

sleep 10800s;

warn "Triggering ipf full 01 task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-ipf.git",
  "challenge_repo_head_ref": "challenges/ipf-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/ipf-full-01",
  "fuzz_tooling_project_name": "ipf",
  "duration": 10800,
  "harnesses_included":false
}'

sleep 30s;

warn "Triggering freerdp full 01 task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "git@github.com:aixcc-finals/afc-freerdp.git",
  "challenge_repo_head_ref": "challenges/fp-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/fp-full-01",
  "fuzz_tooling_project_name": "freerdp",
  "duration": 18000
}'

sleep 30s;

warn "Triggering commons compress full to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "https://github.com/aixcc-finals/afc-commons-compress.git",
    "challenge_repo_head_ref": "challenges/cc-full-01",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/cc-full-01",
    "fuzz_tooling_project_name": "apache-commons-compress",
    "duration": 9000
}'

sleep 30s;

warn "Triggering dropbear full to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "git@github.com:aixcc-finals/afc-dropbear.git",
  "challenge_repo_head_ref": "challenges/db-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/db-full-01",
  "fuzz_tooling_project_name": "dropbear",
  "duration": 12000
}'

sleep 30s;

warn "Triggering zookeeper full to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-zookeeper.git",
  "challenge_repo_head_ref": "challenges/zk-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/zk-full-01",
  "fuzz_tooling_project_name": "zookeeper",
  "duration": 36000
}'

# Sleep for 3 minutes. Let our CRS API receive the full task. Can take anywhere from 15 seconds to 2.5 minutes.
warn "Sleeping for 3 minutes before issuing zookeeper full sarif."
sleep 3m;

# Get the TASK_ID of the full scan from the API_LOGS
dbug "Looking in $API_LOG for TASK_ID"
TASK_ID=$(grep -oP '\(:TASK_ID "\K[^"]+(?="\))' "$API_LOG" | tail -n 1)
dbug "TASK_ID is $TASK_ID"

# Build payload using task ID and sarif data
SARIF_FILE="${testdir}/zookeeper-full-01-sarif.json"
SARIF_DATA=$(cat "$SARIF_FILE")
dbug "sarif data is $SARIF_DATA"
SARIF_PAYLOAD=$(jq -n --arg task_id "$TASK_ID" --argjson sarif "$SARIF_DATA" '{task_id: $task_id, sarif: $sarif}')

# Use the TASK_ID in the trigger sarif curl command.
warn "Triggering tika delta 02 sarif."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/sarif' -H 'Content-Type: application/json' -d "$SARIF_PAYLOAD"
