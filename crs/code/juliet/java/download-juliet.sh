#!/usr/bin/env bash
set -e -u -o pipefail
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
pushd ${SCRIPT_DIR}

JULIET_ZIP=juliet-v1-3.zip
JULIET_PATH=./juliet

# download the juliet test suite for java
curl -o "${JULIET_ZIP}" https://samate.nist.gov/SARD/downloads/test-suites/2017-10-01-juliet-test-suite-for-java-v1-3.zip

# unzip the test suite
unzip "${JULIET_ZIP}" -d "${JULIET_PATH}"

popd
