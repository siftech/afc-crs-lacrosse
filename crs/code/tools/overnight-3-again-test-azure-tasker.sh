#!/usr/bin/env bash
# This will execute a test suite against our azure CRS using the AIxCC provided azure tasker.
# Remake of overnight 3 with the use of 'test-pixel-phantom-api' tasker and not the go time 'api'
# This will trigger the round 3 challenge problems a tika sarif.
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

warn "Triggering curl full 01 task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-curl.git",
  "challenge_repo_head_ref": "challenges/cu-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/cu-full-01",
  "fuzz_tooling_project_name": "curl",
  "duration": 10800
}'

sleep 30s;

warn "Triggering tika full 01 task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-tika.git",
  "challenge_repo_head_ref": "challenges/tk-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/tk-full-01",
  "fuzz_tooling_project_name": "tika",
  "duration": 36000
}'

sleep 10800s;

warn "Triggering tika delta 02 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-tika.git",
  "challenge_repo_base_ref": "87c62bccc3a6fd0343df073511fc520a235618b3",
  "challenge_repo_head_ref": "challenges/tk-delta-02",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/tk-delta-02",
  "fuzz_tooling_project_name": "tika",
  "duration": 18000
}'

# Sleep for 3 minutes. Let our CRS API receive the full task. Can take anywhere from 15 seconds to 2.5 minutes.
warn "Sleeping for 3 minutes before issuing tika delta 02 sarif."
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
SARIF_FILE="${testdir}/tika-delta-02-sarif.json"
SARIF_DATA=$(cat "$SARIF_FILE")
dbug "sarif data is $SARIF_DATA"
SARIF_PAYLOAD=$(jq -n --arg task_id "$TASK_ID" --argjson sarif "$SARIF_DATA" '{task_id: $task_id, sarif: $sarif}')

# Use the TASK_ID in the trigger sarif curl command.
warn "Triggering tika delta 02 sarif."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/sarif' -H 'Content-Type: application/json' -d "$SARIF_PAYLOAD"

sleep 3m;

warn "Triggering libexif delta 01 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-libexif.git",
  "challenge_repo_base_ref": "ffcdfbeb5539c25b1630ba59abf8a22587657adc",
  "challenge_repo_head_ref": "challenges/ex-delta-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/ex-delta-01",
  "fuzz_tooling_project_name": "libexif",
  "duration": 18000
}'

sleep 30s;

warn "Triggering curl delta 01 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-curl.git",
  "challenge_repo_base_ref": "a29184fc5f9b1474c08502d1545cd90375fadd51",
  "challenge_repo_head_ref": "challenges/cu-delta-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/cu-delta-01",
  "fuzz_tooling_project_name": "curl",
  "duration": 18000
}'

sleep 30s;

warn "Triggering s2n-tls full 01 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-s2n-tls.git",
  "challenge_repo_head_ref": "challenges/s2n_tls-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/s2n_tls-full-01",
  "fuzz_tooling_project_name": "s2n-tls",
  "duration": 18000
}'

sleep 30s;

warn "Triggering libpostal full 01 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-libpostal.git",
  "challenge_repo_head_ref": "challenges/libpostal-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/libpostal-full-01",
  "fuzz_tooling_project_name": "libpostal",
  "duration": 18000
}'

sleep 30s;

warn "Triggering tika delta 04 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-tika.git",
  "challenge_repo_base_ref": "30284a3eb45eddd5b812eca12254a99551671f32",
  "challenge_repo_head_ref": "challenges/tk-delta-04",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/tk-delta-04",
  "fuzz_tooling_project_name": "tika",
  "duration": 18000
}'

sleep 18000s;

warn "Triggering tika delta 05 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-tika.git",
  "challenge_repo_base_ref": "4d5194b7d13494f97b89c859282342f5efad9cef",
  "challenge_repo_head_ref": "challenges/tk-delta-05",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/tk-delta-05",
  "fuzz_tooling_project_name": "tika",
  "duration": 18000
}'

sleep 30s;

warn "Triggering tika delta 03 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-tika.git",
  "challenge_repo_base_ref": "08dabf212d551b27de70d3be0653a226e85b1b73",
  "challenge_repo_head_ref": "challenges/tk-delta-03",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/tk-delta-03",
  "fuzz_tooling_project_name": "tika",
  "duration": 18000
}'

sleep 30s;

warn "Triggering tika delta 01 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-tika.git",
  "challenge_repo_base_ref": "d0e3069a8e51554083c2980974f869337b4d6d39",
  "challenge_repo_head_ref": "challenges/tk-delta-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/tk-delta-01",
  "fuzz_tooling_project_name": "tika",
  "duration": 18000
}'

sleep 30s;

warn "Triggering sqlite delta 03 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-sqlite3.git",
  "challenge_repo_base_ref": "35af1ffb5dd21ae47332577c2b6c889da302b497",
  "challenge_repo_head_ref": "challenges/sq-delta-03",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/sq-delta-03",
  "fuzz_tooling_project_name": "sqlite3",
  "duration": 18000
}'
