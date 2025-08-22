#!/usr/bin/env bash
# This will execute a test suite against our azure CRS using the AIxCC provided azure tasker.
# This will trigger 3 ctasks for 5 hours, sleep while the tests run, then trigger 3 more ctasks.
# Anticipated to be used in an azure over nighter.

set -e

PROGNAME=$(basename "$0")
warn()  { echo "$PROGNAME: $*" 1>&2; }
die()   { warn "${@}"; exit 1; }
dbug()   { test -z "$DEBUG" || warn "${@}"; }

if [ -z "${COMPETITION_API_TEAM_ID+x}" ]; then
    die "COMPETITION_API_TEAM_ID undefined"
fi

if [ -z "${COMPETITION_API_TEAM_SECRET+x}" ]; then
    die "COMPETITION_API_TEAM_SECRET undefined"
fi

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

warn "Triggering sqlite3 full 01 task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-sqlite3.git",
  "challenge_repo_head_ref": "challenges/sq-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/sq-full-01",
  "fuzz_tooling_project_name": "sqlite3",
  "duration": 18000
}'

sleep 30s;

warn "Triggering libxml2 full 01 task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "git@github.com:aixcc-finals/afc-libxml2.git",
  "challenge_repo_head_ref": "challenges/lx-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/lx-full-01",
  "fuzz_tooling_project_name": "libxml2",
  "duration": 18000
}'

sleep 18600s;

warn "Triggering sqlite3 delta 01 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-sqlite3.git",
  "challenge_repo_base_ref": "6a3e7f57f00f0a2b6b89b0db7990e3df47175372",
  "challenge_repo_head_ref": "challenges/sq-delta-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/sq-delta-01",
  "fuzz_tooling_project_name": "sqlite3",
  "duration": 18000
}'

sleep 30s;

warn "Triggering zookeeper delta 01 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-zookeeper.git",
  "challenge_repo_base_ref": "f6f34f6d5b6d67205c34de617a0b99fe11e3d323",
  "challenge_repo_head_ref": "challenges/zk-delta-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/zk-delta-01",
  "fuzz_tooling_project_name": "zookeeper",
  "duration": 18000
}'

sleep 30s;

warn "Triggering commons compress delta 02 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-commons-compress.git",
  "challenge_repo_base_ref": "154edd0066d1aaf18daafb88253cacbf39017d61",
  "challenge_repo_head_ref": "challenges/cc-delta-02",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/cc-delta-02",
  "fuzz_tooling_project_name": "apache-commons-compress",
  "duration": 18000
}'
