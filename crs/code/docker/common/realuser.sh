#!/usr/bin/env sh

WHO=/realuser

PROGNAME=$(basename "$0")
warn()  { echo "$PROGNAME: ${@}" 1>&2; }
die()   { warn "${@}"; exit 1; }
dbug()   { test -z $DEBUG || warn "${@}"; }

# example usage:
   #dbug This only prints if DEBUG is defined
   #test -e foo || die file foo must exist
   #test -z $FOO && die Environment variable FOO must be defined

test -z "${HOST_USERNAME}" && die You must set HOST_USERNAME before calling realuser.sh
test -z "${HOST_GROUPNAME}" && die You must set HOST_GROUPNAME before calling realuser.sh

  # in old way, a dir was mounted at /realuser; in new way, env vars bring in uid/gid info.
  # for temporary backward compat, we'll only override the env vars if they arent set.

if test -z "${HOST_USERID}" ; then 
  echo "Relying on old $WHO mount for uid/gid; this fails on mac/pc"
  if ! stat $WHO > /dev/null ; then echo "You must mount a directory to $WHO in order to properly set up realuser in docker" ; exit 1; fi
  USERID=$(stat -c %u $WHO)
  GROUPID=$(stat -c %g $WHO)
else
  USERID=$HOST_USERID
  GROUPID=$HOST_GROUPID
fi

dbug HOST_USERNAME is $HOST_USERNAME
dbug USERID is $USERID
dbug HOST_GROUPNAME is $HOST_GROUPNAME
dbug GROUPID is $GROUPID

	# this flock and the companion close-paren stuff locks out other concurrent /realuser.sh 
	# from this critical section, so that no races on user/account mgmt occur
(flock 9 || exit 1

# If user already exists (eg, this is running in a docker exec call), don't recreate the user.
if ! [ -e /home/$HOST_USERNAME ]; then
    deluser $HOST_USERNAME > /dev/null 2>&1
    groupadd -o -g $GROUPID $HOST_GROUPNAME > /dev/null 2>&1
    useradd -o -u $USERID -g $HOST_GROUPNAME -s /bin/bash --home-dir /home/$HOST_USERNAME --create-home --skel /home/realuser $HOST_USERNAME > /dev/null 2>&1
    adduser $HOST_USERNAME sudo > /dev/null 2>&1
    if which docker > /dev/null; then
        groupdel docker > /dev/null 2>&1
        groupadd -o -g $HOST_DOCKERGROUPID docker > /dev/null 2>&1
        adduser $HOST_USERNAME docker > /dev/null 2>&1
        chgrp docker /var/run/docker.sock     # above stuff not enuf on windows boxes, so override it all!
    fi
    echo $HOST_USERNAME:sift | chpasswd
fi


# 04-18-18 MAD
# If user starting container is user id 1000, they will always
# be given the name realuser in the container because it gets created as
# user 1000. Delete realuser so that a person gets assigned their
# own username in the container and future containers get named
# correctly 
if [ `id -u realuser 2>/dev/null || echo -1` -ge 0 ]; then 
    deluser realuser > /dev/null 2>&1
fi
) 9>/tmp/${HOST_USERNAME}_realuserlockfile

chmod -f 666 /tmp/${HOST_USERNAME}_realuserlockfile
gosu $HOST_USERNAME "$@"
