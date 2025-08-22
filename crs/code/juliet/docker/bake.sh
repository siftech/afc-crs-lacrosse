#!/usr/bin/env bash
set -e -u -o pipefail
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
pushd ${SCRIPT_DIR}
docker buildx bake -f docker-bake.hcl --load $@
popd