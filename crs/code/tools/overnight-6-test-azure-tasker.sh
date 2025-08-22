#!/usr/bin/env bash
# This will execute a test suite against our azure CRS using the AIxCC provided azure tasker.
# This will trigger 3 ctasks for 5 hours, sleep while the tests run, then trigger 3 more ctasks.
# Anticipated to be used in an azure over nighter.

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

warn "Triggering dropbear full to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "git@github.com:aixcc-finals/afc-dropbear.git",
  "challenge_repo_head_ref": "challenges/db-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/db-full-01",
  "fuzz_tooling_project_name": "dropbear",
  "duration": 10800
}'

sleep 30

warn "Triggering freerdp full 01 task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "git@github.com:aixcc-finals/afc-freerdp.git",
  "challenge_repo_head_ref": "challenges/fp-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/fp-full-01",
  "fuzz_tooling_project_name": "freerdp",
  "duration": 10800
}'

sleep 30

warn "Triggering sqlite3 full 01 task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-sqlite3.git",
  "challenge_repo_head_ref": "challenges/sq-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/sq-full-01",
  "fuzz_tooling_project_name": "sqlite3",
  "duration": 10800
}'
# Sleep for 3 minutes. Let our CRS API receive the full task. Can take anywhere from 15 seconds to 2.5 minutes.
warn "Sleeping for 20 minutes before issuing sqlite3 full sarif."
sleep 20m

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
SARIF_FILE="${testdir}/sqlite3-full-false-desc.json"
SARIF_DATA=$(cat "$SARIF_FILE")
dbug "sarif data is $SARIF_DATA"
SARIF_PAYLOAD=$(jq -n --arg task_id "$TASK_ID" --argjson sarif "$SARIF_DATA" '{task_id: $task_id, sarif: $sarif}')

# Use the TASK_ID in the trigger sarif curl command.
warn "Triggering sqlite full sarif."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/sarif' -H 'Content-Type: application/json' -d "$SARIF_PAYLOAD"

sleep 10800

warn "Triggering libxml2 full 01 task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "git@github.com:aixcc-finals/afc-libxml2.git",
  "challenge_repo_head_ref": "challenges/lx-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/lx-full-01",
  "fuzz_tooling_project_name": "libxml2",
  "duration": 10800
}'

sleep 30

warn "Triggering commons compress delta 03 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-commons-compress.git",
  "challenge_repo_base_ref": "6e608498013784abb6878cad7906c2ddc41e45f1",
  "challenge_repo_head_ref": "challenges/cc-delta-03",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/cc-delta-03",
  "fuzz_tooling_project_name": "apache-commons-compress",
  "duration": 3600
}'

sleep 30

warn "Triggering freerdp delta to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "git@github.com:aixcc-finals/afc-freerdp.git",
  "challenge_repo_base_ref": "a92cc0f3ebc3d3f4cf5b6097920a391e9b5fcfcf",
  "challenge_repo_head_ref": "challenges/fp-delta-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/fp-delta-01",
  "fuzz_tooling_project_name": "freerdp",
  "duration": 7200
}'

sleep 30

warn "Triggering sqlite3 delta 01 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-sqlite3.git",
  "challenge_repo_base_ref": "6a3e7f57f00f0a2b6b89b0db7990e3df47175372",
  "challenge_repo_head_ref": "challenges/sq-delta-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/sq-delta-01",
  "fuzz_tooling_project_name": "sqlite3",
  "duration": 7200
}'

# Sleep for 3 minutes. Let our CRS API receive the full task. Can take anywhere from 15 seconds to 2.5 minutes.
warn "Sleeping for 20 minutes before issuing sqlite3 full sarif."
sleep 20m

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
warn "Triggering sqlite full sarif."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/sarif' -H 'Content-Type: application/json' -d "$SARIF_PAYLOAD"

sleep 7200

warn "Triggering zookeeper delta 01 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-zookeeper.git",
  "challenge_repo_base_ref": "f6f34f6d5b6d67205c34de617a0b99fe11e3d323",
  "challenge_repo_head_ref": "challenges/zk-delta-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/zk-delta-01",
  "fuzz_tooling_project_name": "zookeeper",
  "duration": 7200
}'

sleep 30

warn "Triggering commons compress delta 02 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-commons-compress.git",
  "challenge_repo_base_ref": "154edd0066d1aaf18daafb88253cacbf39017d61",
  "challenge_repo_head_ref": "challenges/cc-delta-02",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/cc-delta-02",
  "fuzz_tooling_project_name": "apache-commons-compress",
  "duration": 3600
}'

sleep 30

warn "Triggering unharnessed libxml2 full 01 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "git@github.com:aixcc-finals/afc-libxml2.git",
  "challenge_repo_head_ref": "challenges/lx-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/lx-full-01",
  "fuzz_tooling_project_name": "libxml2",
  "duration": 3600,
  "harnesses_included":false
}'

sleep 30

warn "Triggering integretion delta 01 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/integration-test.git",
  "challenge_repo_base_ref": "4a714359c60858e3821bd478dc846de1d04dc977",
  "challenge_repo_head_ref": "challenges/integration-test-delta-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/integration-test-delta-01",
  "fuzz_tooling_project_name": "integration-test",
  "duration": 7200
}'

sleep 7200

warn "Triggering libpng delta 01 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "git@github.com:aixcc-finals/example-libpng.git",
  "challenge_repo_base_ref": "5bf8da2d7953974e5dfbd778429c3affd461f51a",
  "challenge_repo_head_ref": "challenges/lp-delta-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/lp-delta-01",
  "fuzz_tooling_project_name": "libpng",
  "duration": 10800
}'

sleep 30

warn "Triggering sqlite3 delta 02 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-sqlite3.git",
  "challenge_repo_base_ref": "35af1ffb5dd21ae47332577c2b6c889da302b497",
  "challenge_repo_head_ref": "challenges/sq-delta-02",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/sq-delta-02",
  "fuzz_tooling_project_name": "sqlite3",
  "duration": 7200
}'

sleep 30

warn "Triggering sqlite3 delta 03 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-sqlite3.git",
  "challenge_repo_base_ref": "35af1ffb5dd21ae47332577c2b6c889da302b497",
  "challenge_repo_head_ref": "challenges/sq-delta-03",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/sq-delta-03",
  "fuzz_tooling_project_name": "sqlite3",
  "duration": 7200
}'

sleep 30

warn "Triggering commons compress full 01 task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-commons-compress.git",
   "challenge_repo_head_ref": "challenges/cc-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/cc-full-01",
  "fuzz_tooling_project_name": "apache-commons-compress",
  "duration": 10800
}'

sleep 10800
