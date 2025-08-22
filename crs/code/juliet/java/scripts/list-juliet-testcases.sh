#!/usr/bin/env bash
set -e -o pipefail
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
pushd ${SCRIPT_DIR}

if [[ -z "${JULIET_PATH}" ]]; then
  JULIET_PATH="${SCRIPT_DIR}/../juliet/Java"
fi

TESTCASES_DIR="${JULIET_PATH}/src/testcases"
find $TESTCASES_DIR -type f -name "build.xml" \
    | sed -nr "s|^$TESTCASES_DIR/||p" \
    | sed -nr "s|/.*$||p" \
    | sort -u
popd
