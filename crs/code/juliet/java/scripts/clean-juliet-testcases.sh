#!/usr/bin/env bash
set -e -o pipefail
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

if [[ -z "${JULIET_PATH}" ]]; then
  JULIET_PATH="${SCRIPT_DIR}/../juliet/Java"
fi

clean () {
    find ./src/testcases -type f -name "$1" | xargs rm &> /dev/null || echo "No files with name '$1' to remove"
}

pushd ${JULIET_PATH}
clean "build.xml"
clean "web.xml"
clean "ServletMain.java"
clean "Main.java"
popd
