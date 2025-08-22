#!/usr/bin/env bash

thisdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd -P "$( dirname "$SOURCE" )" && /bin/pwd )"
PROGIMAGE=$(basename "$0")
warn()  { echo "$PROGIMAGE: ${@}" 1>&2; }
die()   { warn "${@}"; exit 1; }
dbug()   { test -z $DEBUG || warn "${@}"; }

# This script is usually called from the cleanup routine, which is
# *not* an agent and doesn't have a logfile, so all output goes to
# console.  Accordingly, output is guarded by dbug.
# Set DEBUG for output to the console.
#DEBUG=1

DEST=$1
dbug "get-container-logs.sh ${DEST}"
#dbug "DEST: ${DEST}"

# Get container logs for all active containers and put them in <dest>/container-logs

# create target dir
CONTAINER_LOGS_PATH="${DEST}/container-logs/"
dbug "Creating ${CONTAINER_LOGS_PATH}"
mkdir -p ${CONTAINER_LOGS_PATH}

# Get the list of active container IDs
#CONTAINER_IDS=$(docker ps -q)

# Get the list of container Names.
CONTAINER_NAMES=$(docker ps --all --format "{{.Names}}")

# Iterate over each container names and save the logs
for CNAME in ${CONTAINER_NAMES}; do
    LOG_PATH="${CONTAINER_LOGS_PATH}/${CNAME}.log"
    docker logs "${CNAME}" >& "${LOG_PATH}"
    dbug "Logs for container ${CNAME} saved to ${LOG_PATH}"
done
