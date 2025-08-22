#!/usr/bin/env bash
# This will execute a test suite against our azure CRS using the AIxCC provided azure tasker.
# This will trigger an additional set of round 3 challenge problems.
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

#   "ex3-cc-delta-02",
#   "ex3-cc-delta-03",
#   "ex3-cc-full-01",
#   "ex3-db-full-01",
#   "ex3-fp-delta-01",
#   "ex3-fp-full-01",
#   "ex3-integration-test-delta-01",
#   "ex3-integration-test-unharnessed-delta-01",
#   "ex3-lp-delta-01",
#   "ex3-lx-delta-01",
#   "ex3-lx-delta-02",
#   "ex3-sq-delta-01",
#   "ex3-sq-delta-02",
#   "ex3-sq-delta-03",
#   "ex3-sq-full-01",
#   "ex3-tk-full-01",
#   "ex3-tk-delta-01",
#   "ex3-tk-delta-02",
#   "ex3-tk-delta-03",
#   "ex3-tk-delta-04",
#   "ex3-tk-delta-05",
#   "ex3-zk-delta-01",
#   "ex3-zk-delta-02",
#   "ex3-zk-full-01",
#   "ex3-cu-full-01",
#   "ex3-cu-delta-01",
#   "ex3-ex-delta-01",
#   "ex3-libpostal-full-01",
#   "ex3-s2n_tls-full-01",
#   "ex3-ipf-full-01",
#   "delta"

warn "Triggering common compress full 01 task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://api.aixcc.tech/v1/request/ex3-cc-full-01/' -d '{"duration_secs":10800}'
sleep 30s;

warn "Triggering dropbear full 01 task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://api.aixcc.tech/v1/request/ex3-db-full-01/' -d '{"duration_secs":10800}'
sleep 30s;

warn "Triggering freerdp full 01 task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://api.aixcc.tech/v1/request/ex3-fp-full-01/' -d '{"duration_secs":10800}'
sleep 30s;

sleep 10800s;

warn "Triggering libxml delta 02 task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://api.aixcc.tech/v1/request/ex3-lx-delta-02/' -d '{"duration_secs":10800}'
sleep 30s;

warn "Triggering sqlite3 delta 02 task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://api.aixcc.tech/v1/request/ex3-sq-delta-02/' -d '{"duration_secs":10800}'
sleep 30s;

warn "Triggering zookeeper delta 02 task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://api.aixcc.tech/v1/request/ex3-zk-delta-02/' -d '{"duration_secs":10800}'
sleep 30s;

warn "Triggering tika delta 02 task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://api.aixcc.tech/v1/request/ex3-tk-delta-02/' -d '{"duration_secs":2700}'
sleep 30s;

sleep 10800s;

warn "Triggering libpng delta 01 task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://api.aixcc.tech/v1/request/ex3-lp-delta-01/' -d '{"duration_secs":10800}'
sleep 30s;

warn "Triggering libxml full 01 task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://api.aixcc.tech/v1/request/ex3-lx-delta-01/' -d '{"duration_secs":1800}'
sleep 30s;

warn "Triggering freerdp delta 01 task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://api.aixcc.tech/v1/request/ex3-fp-delta-01/' -d '{"duration_secs":10800}'
sleep 30s;

warn "Triggering curl delta 01 task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://api.aixcc.tech/v1/request/ex3-cu-delta-01/' -d '{"duration_secs":10800}'
sleep 30s;

sleep 16200s;

warn "Triggering common compress delta 03 task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://api.aixcc.tech/v1/request/ex3-cc-delta-03/' -d '{"duration_secs":1250}'
sleep 1800s;

warn "Triggering zookeeper full 01 task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://api.aixcc.tech/v1/request/ex3-zk-full-01/' -d '{"duration_secs":10800}'
sleep 30s;

warn "Triggering common compress delta 02 task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://api.aixcc.tech/v1/request/ex3-cc-delta-02/' -d '{"duration_secs":10800}'
sleep 30s;

warn "Triggering sqlite3 full 01 task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://api.aixcc.tech/v1/request/ex3-sq-full-01/' -d '{"duration_secs":10800}'
sleep 30s;

warn "Triggering libpostal full 01 task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://api.aixcc.tech/v1/request/ex3-libpostal-full-01/' -d '{"duration_secs":10800}'
sleep 30s;

warn "Triggering tika delta 05 task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://api.aixcc.tech/v1/request/ex3-tk-delta-05/' -d '{"duration_secs":2700}'
sleep 30s;
