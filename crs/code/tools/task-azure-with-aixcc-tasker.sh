#!/usr/bin/env bash
PROGNAME=$(basename "$0")
warn()  { echo "$PROGNAME: ${@}" 1>&2; }
die()   { warn "${@}"; exit 1; }
dbug()   { test -z $DEBUG || warn "${@}"; }

thisdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd -P "$( dirname "$SOURCE" )" && /bin/pwd )"

OPT=$1
dbug "Option is $OPT"

if [ -z "${COMPETITION_API_TEAM_ID+x}" ]; then
    die "COMPETITION_API_TEAM_ID undefined"
fi

if [ -z "${COMPETITION_API_TEAM_SECRET+x}" ]; then
    die "COMPETITION_API_TEAM_SECRET undefined"
fi

if [[ "$OPT" == "libpng-delta" ]]; then
    warn "Triggering libpng delta task to our Azure lax neverending."
    curl -u ${COMPETITION_API_TEAM_ID}:${COMPETITION_API_TEAM_SECRET} -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
        "challenge_repo_url": "git@github.com:aixcc-finals/example-libpng.git",
        "challenge_repo_base_ref": "0cc367aaeaac3f888f255cee5d394968996f736e",
        "challenge_repo_head_ref": "fdacd5a1dcff42175117d674b0fda9f8a005ae88",
        "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
        "fuzz_tooling_ref": "d5fbd68fca66e6fa4f05899170d24e572b01853d",
        "fuzz_tooling_project_name": "libpng",
        "duration": 3600
    }'
    sleep 1
    exit 0
fi
if [[ "$OPT" == "libpng-full" ]]; then
    warn "Triggering libpng full task to our Azure lax neverending."
    curl -u ${COMPETITION_API_TEAM_ID}:${COMPETITION_API_TEAM_SECRET} -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
        "challenge_repo_url": "git@github.com:aixcc-finals/example-libpng.git",
        "challenge_repo_head_ref": "fdacd5a1dcff42175117d674b0fda9f8a005ae88",
        "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
        "fuzz_tooling_ref": "v1.0.0",
        "fuzz_tooling_project_name": "libpng",
        "duration": 3600
    }'
    sleep 1
    exit 0
fi
if [[ "$OPT" == "zookeeper-delta" ]]; then
    warn "Triggering zookeeper delta task to our Azure lax neverending."
    curl -u ${COMPETITION_API_TEAM_ID}:${COMPETITION_API_TEAM_SECRET} -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
        "challenge_repo_url": "git@github.com:aixcc-finals/afc-zookeeper.git",
        "challenge_repo_base_ref": "d19cef9ca254a4c1461490ed8b82ffccfa57461d",
        "challenge_repo_head_ref": "5ee4f185d0431cc88f365ce779aa04a87fe7690f",
        "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
        "fuzz_tooling_ref": "challenge-state/zk-ex1-delta-01",
        "fuzz_tooling_project_name": "zookeeper",
        "duration": 3600
    }'
    sleep 1
    exit 0
fi
if [[ "$OPT" == "zookeeper-full" ]]; then
    warn "Triggering zookeeper full task to our Azure lax neverending."
    curl -u ${COMPETITION_API_TEAM_ID}:${COMPETITION_API_TEAM_SECRET} -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
        "challenge_repo_url": "https://github.com/aixcc-finals/afc-zookeeper.git",
        "challenge_repo_head_ref": "5ee4f185d0431cc88f365ce779aa04a87fe7690f",
        "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
        "fuzz_tooling_ref": "challenge-state/zk-ex1-delta-01",
        "fuzz_tooling_project_name": "zookeeper",
        "duration": 3600
    }'
    sleep 1
    exit 0
fi
if [[ "$OPT" == "libxml2-delta" ]]; then
    warn "Triggering libxml2 delta task to our Azure lax neverending."
    curl -u ${COMPETITION_API_TEAM_ID}:${COMPETITION_API_TEAM_SECRET} -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
        "challenge_repo_url": "git@github.com:aixcc-finals/afc-libxml2.git",
        "challenge_repo_base_ref": "792cc4a1462d4a969d9d38bd80a52d2e4f7bd137",
        "challenge_repo_head_ref": "9d1cb67c31933ee5ae3ee458940f7dbeb2fde8b8",
        "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
        "fuzz_tooling_ref": "challenge-state/lx-ex1-delta-01",
        "fuzz_tooling_project_name": "libxml2",
        "duration": 3600
    }'
    sleep 1
    exit 0
fi
if [[ "$OPT" == "libxml2-full" ]]; then
    warn "Triggering libxml2 full task to our Azure lax neverending."
    curl -u ${COMPETITION_API_TEAM_ID}:${COMPETITION_API_TEAM_SECRET} -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
        "challenge_repo_url": "git@github.com:aixcc-finals/afc-libxml2.git",
        "challenge_repo_head_ref": "9d1cb67c31933ee5ae3ee458940f7dbeb2fde8b8",
        "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
        "fuzz_tooling_ref": "challenge-state/lx-ex1-delta-01",
        "fuzz_tooling_project_name": "libxml2",
        "duration": 3600
    }'
    sleep 1
    exit 0
fi
if [[ "$OPT" == "dropbear-full" ]]; then
    warn "Triggering dropbear full task to our Azure lax neverending."
    curl -u ${COMPETITION_API_TEAM_ID}:${COMPETITION_API_TEAM_SECRET} -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
        "challenge_repo_url": "git@github.com:aixcc-finals/afc-dropbear.git",
        "challenge_repo_head_ref": "challenges/db-full-01",
        "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
        "fuzz_tooling_ref": "challenge-state/db-full-01",
        "fuzz_tooling_project_name": "dropbear",
        "duration": 3600
    }'
    sleep 1
    exit 0
fi
if [[ "$OPT" == "zookeeper-full" ]]; then
    warn "Triggering zookeeper full task to our Azure lax neverending."
    curl -u ${COMPETITION_API_TEAM_ID}:${COMPETITION_API_TEAM_SECRET} -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
        "challenge_repo_url": "https://github.com/aixcc-finals/afc-zookeeper.git",
        "challenge_repo_head_ref": "challenges/zk-full-01",
        "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
        "fuzz_tooling_ref": "challenge-state/zk-full-01",
        "fuzz_tooling_project_name": "zookeeper",
        "duration": 3600
    }'
    sleep 1
    exit 0
fi
if [[ "$OPT" == "zookeeper-delta01" ]]; then
    warn "Triggering zookeeper delta 01 task to our Azure lax neverending."
    curl -u ${COMPETITION_API_TEAM_ID}:${COMPETITION_API_TEAM_SECRET} -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
        "challenge_repo_url": "https://github.com/aixcc-finals/afc-zookeeper.git",
        "challenge_repo_head_ref": "5ee4f185d0431cc88f365ce779aa04a87fe7690f",
        "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
        "fuzz_tooling_ref": "challenge-state/zk-ex1-delta-01",
        "fuzz_tooling_project_name": "zookeeper",
        "duration": 3600
    }'
    sleep 1
    exit 0
fi
if [[ "$OPT" == "freerdp-delta" ]]; then
    warn "Triggering freerdp delta 01 task to our Azure lax neverending."
    curl -u ${COMPETITION_API_TEAM_ID}:${COMPETITION_API_TEAM_SECRET} -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
        "challenge_repo_url": "git@github.com:aixcc-finals/afc-freerdp.git",
        "challenge_repo_base_ref": "a92cc0f3ebc3d3f4cf5b6097920a391e9b5fcfcf",
        "challenge_repo_head_ref": "challenges/fp-delta-01",
        "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
        "fuzz_tooling_ref": "challenge-state/fp-delta-01",
        "fuzz_tooling_project_name": "freerdp",
        "duration": 3600
    }'
    sleep 1
    exit 0
fi
if [[ "$OPT" == "freerdp-full" ]]; then
    warn "Triggering freerdp full task to our Azure lax neverending."
    curl -u ${COMPETITION_API_TEAM_ID}:${COMPETITION_API_TEAM_SECRET} -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
        "challenge_repo_url": "git@github.com:aixcc-finals/afc-freerdp.git",
        "challenge_repo_head_ref": "challenges/fp-full-01",
        "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
        "fuzz_tooling_ref": "challenge-state/fp-full-01",
        "fuzz_tooling_project_name": "freerdp",
        "duration": 3600
    }'
    sleep 1
    exit 0
fi
if [[ "$OPT" == "commons-compress-full" ]]; then
    warn "Triggering commons compress full task to our Azure lax neverending."
    curl -u ${COMPETITION_API_TEAM_ID}:${COMPETITION_API_TEAM_SECRET} -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
        "challenge_repo_url": "https://github.com/aixcc-finals/afc-commons-compress.git",
        "challenge_repo_head_ref": "challenges/cc-full-01",
        "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
        "fuzz_tooling_ref": "challenge-state/cc-full-01",
        "fuzz_tooling_project_name": "apache-commons-compress",
        "duration": 3600
    }'
    sleep 1
    exit 0
fi
if [[ "$OPT" == "commons-compress-delta" ]]; then
    warn "Triggering commons compress delta task to our Azure lax neverending."
    curl -u ${COMPETITION_API_TEAM_ID}:${COMPETITION_API_TEAM_SECRET} -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
        "challenge_repo_url": "https://github.com/aixcc-finals/afc-commons-compress.git",
        "challenge_repo_head_ref": "challenges/cc-delta-03",
        "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
        "fuzz_tooling_ref": "challenge-state/cc-delta-03",
        "fuzz_tooling_project_name": "apache-commons-compress",
        "duration": 3600
    }'
    sleep 1
    exit 0
fi
if [[ "$OPT" == "sqlite3-full" ]]; then
    warn "Triggering sqlite3 full task to our Azure lax neverending."
    curl -u ${COMPETITION_API_TEAM_ID}:${COMPETITION_API_TEAM_SECRET} -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
        "challenge_repo_url": "https://github.com/aixcc-finals/afc-sqlite3.git",
        "challenge_repo_head_ref": "challenges/sq-full-01",
        "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
        "fuzz_tooling_ref": "challenge-state/sq-full-01",
        "fuzz_tooling_project_name": "sqlite3",
        "duration": 3600
    }'
    sleep 1
    exit 0
fi
if [[ "$OPT" == "sqlite3-delta" ]]; then
    warn "Triggering sqlite3 delta task to our Azure lax neverending."
    curl -u ${COMPETITION_API_TEAM_ID}:${COMPETITION_API_TEAM_SECRET} -X 'POST' 'https://pixel-phantom.tasker.aixcc.tech/webhook/trigger_task' -H 'Content-Type: application/json' -d '{
        "challenge_repo_url": "https://github.com/aixcc-finals/afc-sqlite3.git",
        "challenge_repo_base_ref": "6a3e7f57f00f0a2b6b89b0db7990e3df47175372",
        "challenge_repo_head_ref": "challenges/sq-delta-01",
        "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
        "fuzz_tooling_ref": "challenge-state/sq-delta-01",
        "fuzz_tooling_project_name": "sqlite3",
        "duration": 3600
    }'
    sleep 1
    exit 0
fi
