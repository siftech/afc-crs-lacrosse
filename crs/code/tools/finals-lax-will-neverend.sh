#!/usr/bin/env bash
DEBUG=1

# Usual flags
PROGNAME=$(basename "$0")
warn()  { echo "$PROGNAME: $*" 1>&2; }
dbug()   { test -z $DEBUG || warn "${@}"; }
die()   { warn "${@}"; exit 1; }

# Get tools(thisdir) and testdir
thisdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd -P "$( dirname "$SOURCE" )" && /bin/pwd )"
testdir="${thisdir}/../test"
dbug "thisdir is $thisdir"
dbug "testdir is $testdir"

# Check hostname, if hostname is a fuzzbomb, do not run the neverending. Only primary host should run the prt.
OPTI_HOST_NAME="pixel-phantom"
dbug "OPTI_HOST_NAME is $OPTI_HOST_NAME"

CHECK_HOST_NAME="$(hostname --short)"
dbug "CHECK_HOST_NAME is $CHECK_HOST_NAME"

if [[ "$OPTI_HOST_NAME" != "$CHECK_HOST_NAME" ]]; then
  die "Hostname: \"$CHECK_HOST_NAME\" is not \"$OPTI_HOST_NAME\""
fi

pushd "$testdir" || exit
run_neverending() {
  DEBUG=1 "${thisdir}/../prt/prt" "${testdir}/lax-neverending.prt"
}

# Give all machines time to start
sleep 300

while true; do
  warn "Starting a lax neverending that will restart forever..."

  dbug "exec DEBUG=1 ${thisdir}/../prt/prt ${testdir}/lax-neverending.prt"
  run_neverending
  LAX_EXIT_CODE=$?

  warn "lax-neverending ended with exit code: ${LAX_EXIT_CODE}. Restarting in 90 seconds"
  kill-stray-condor-processes
  sleep 90
done
