#!/usr/bin/env bash
set -e -o pipefail
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

if [[ -z "${JULIET_PATH}" ]]; then
  JULIET_PATH="${SCRIPT_DIR}/../juliet/Java"
fi

pushd ${JULIET_PATH}
./create_per_cwe_files.py "CWE787_"
./create_per_cwe_files.py "CWE79_"
./create_per_cwe_files.py "CWE89_"
./create_per_cwe_files.py "CWE416_"
./create_per_cwe_files.py "CWE78_"
./create_per_cwe_files.py "CWE20_"
./create_per_cwe_files.py "CWE125_"
./create_per_cwe_files.py "CWE22_"
./create_per_cwe_files.py "CWE352_"
./create_per_cwe_files.py "CWE434_"
./create_per_cwe_files.py "CWE862_"
./create_per_cwe_files.py "CWE476_"
./create_per_cwe_files.py "CWE287_"
./create_per_cwe_files.py "CWE190_"
./create_per_cwe_files.py "CWE502_"
./create_per_cwe_files.py "CWE77_"
./create_per_cwe_files.py "CWE119_"
./create_per_cwe_files.py "CWE798_"
./create_per_cwe_files.py "CWE918_"
./create_per_cwe_files.py "CWE306_"
./create_per_cwe_files.py "CWE362_"
./create_per_cwe_files.py "CWE269_"
./create_per_cwe_files.py "CWE94_"
./create_per_cwe_files.py "CWE863_"
./create_per_cwe_files.py "CWE276_"
popd
