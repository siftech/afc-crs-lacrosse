#!/usr/bin/env bash
# This will execute a test suite against our azure CRS using the AIxCC provided azure tasker.
# This will trigger the round 3 challenge problems with two sarifs.
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

warn "Triggering curl full 01 task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://api.aixcc.tech/v1/request/ex3-cu-full-01/' -d '{"duration_secs":10800}'
sleep 30s;

warn "Triggering ipf full 01 task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://api.aixcc.tech/v1/request/ex3-ipf-full-01/' -d '{"duration_secs":10800}'
sleep 30s;

warn "Triggering tika full 01 task to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://api.aixcc.tech/v1/request/ex3-tk-full-01/' -d '{"duration_secs":10800}'

sleep 10800s;

warn "Triggering tika delta 02 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://api.aixcc.tech/v1/request/ex3-tk-delta-02/' -d '{"duration_secs":10800}'
sleep 30s;

warn "Triggering libexif delta 01 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://api.aixcc.tech/v1/request/ex3-ex-delta-01/' -d '{"duration_secs":10800}'
sleep 30s;

warn "Triggering curl delta 01 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://api.aixcc.tech/v1/request/ex3-cu-delta-01/' -d '{"duration_secs":10800}'
sleep 30s;

warn "Triggering s2n-tls full 01 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://api.aixcc.tech/v1/request/ex3-s2n_tls-full-01/' -d '{"duration_secs":10800}'
sleep 30s;

warn "Triggering libpostal full 01 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://api.aixcc.tech/v1/request/ex3-libpostal-full-01/' -d '{"duration_secs":10800}'
sleep 30s;

warn "Triggering tika delta 04 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://api.aixcc.tech/v1/request/ex3-tk-delta-04/' -d '{"duration_secs":10800}'
sleep 18000s;

warn "Triggering tika delta 05 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://api.aixcc.tech/v1/request/ex3-tk-delta-05/' -d '{"duration_secs":10800}'
sleep 30s;

warn "Triggering tika delta 03 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://api.aixcc.tech/v1/request/ex3-tk-delta-03/' -d '{"duration_secs":10800}'
sleep 30s;

warn "Triggering tika delta 01 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://api.aixcc.tech/v1/request/ex3-tk-delta-01/' -d '{"duration_secs":10800}'
sleep 30s;

warn "Triggering sqlite delta 03 to our Azure lax neverending."
curl -u "${COMPETITION_API_TEAM_ID}":"${COMPETITION_API_TEAM_SECRET}" -X 'POST' 'https://api.aixcc.tech/v1/request/ex3-sq-delta-01/' -d '{"duration_secs":10800}'
