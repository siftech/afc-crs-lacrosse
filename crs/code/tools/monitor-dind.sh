#!/bin/bash

# monitor the dind on host (first arg) and reboot it if needed

# when not flaky(10,10,docker-run-on-host-simple-ls) (cd $logdir; ./reboot-dind-$host.sh)

set -x

PROGNAME=$(basename "$0")
warn()  { echo "$PROGNAME: ${@}" 1>&2; }
die()   { warn "${@}"; exit 1; }
dbug()   { test -z $DEBUG || warn "${@}"; }


# Function to cleanup on script exit
cleanup() {
    die "$0 exiting..."
    exit 0
}

# Set up signal handlers
trap cleanup SIGINT SIGTERM

if [ "$#" == "0" ]; then
	echo "Usage: $0 host"
	exit 1
fi

# example usage:
   #dbug This only prints if DEBUG is defined
   #test -e foo || die file foo must exist
   #test -z $FOO && die Environment variable FOO must be defined

# bash-ism to get location of script.  Must use /bin/pwd to get absolute path.
thisdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd -P "$( dirname "$SOURCE" )" && /bin/pwd )"
dbug thisdir is $thisdir

HOST=$1
dbug HOST arg is $HOST
shift	# drop the first arg

#REBOOT=./reboot-dind-${HOST}.sh
#dbug REBOOT is $REBOOT
#
#LOGDIR=$1
#dbug LOGDIR arg is $LOGDIR
#shift
#
#cd $LOGDIR	# we'll put all the logs where they belong!

# this may be a reasonable test of the dind liveness
# docker exec $USER-dind  /bin/date

while true; do
    # Wait before next check; i moved this to top of loop to give dind time to start at very beginning
    sleep 5m
    #sleep 20s

    # Run the flaky command
    #if $thisdir/sshc $HOST $thisdir/../prt/flaky 2 2  docker exec $USER-dind  /bin/date > ./monitoring-dind-$HOST.log 2>&1; then
	# NOTE sshc seems to be hiding returnval and always succeeding??
    if ssh $HOST $thisdir/../prt/flaky 5 5  docker exec $USER-dind  /bin/date ; then
        dbug "flaky command succeeded"
    else
        exit_code=$?
        echo "flaky command failed with exit code $exit_code "

#        # Check if reboot script exists and is executable
#        if [[ -x $REBOOT ]]; then
#            dbug "Executing $REBOOT script"
#            $REBOOT
#        else
#            dbug "ERROR: executable reboot script $REBOOT not found in current directory"
#        fi
        
	ssh $HOST docker rm -f $USER-dind
	ssh $HOST $thisdir/start-dind &
	#ssh $HOST DOCKER_HOST=unix:///run/user/5009/docker.sock $thisdir/start-dind &
        dbug "Rebooting completed, continuing monitoring"
    fi
    
done
