#!/bin/bash -e
# Script to tag as :latest and sync-to-cluster the Docker images used by NF, 

DOCKER_TAG=${DOCKER_TAG:-${USER}}
dockerdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd -P "$( dirname "$SOURCE" )" && pwd )"
cd ${dockerdir}/../../
nfhome=`/bin/pwd`

for i in $(${dockerdir}/../tools/list-lacrosse-images); do
    	echo "Tagging $i:$DOCKER_TAG with \"latest\"."
    	docker tag $i:$DOCKER_TAG $i:latest
done

# The sync is done in build now, b/c tests run after build need to use the new ver
#for i in $(${dockerdir}/../tools/list-nf-images --sync); do
#	${dockerdir}/../tools/force-sync-docker-image $i:latest
#done

# But we do still need to update the remote tags
for i in $(${dockerdir}/../tools/list-lacrosse-images --sync); do
  for h in $(${dockerdir}/../tools/list-cluster-nodes); do
   if [ $h == `hostname -f` ] ; then
           echo Skipping tag on $h, already done;
   else
    	echo "Tagging $i:$DOCKER_TAG on $h with \"latest\"."
	${dockerdir}/../tools/sshc $h ${nfhome}/rsync/code/tools/docker tag $i:$DOCKER_TAG $i:latest
   fi
  done
done
