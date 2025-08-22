#!/usr/bin/env bash
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
pushd ${SCRIPT_DIR}/..
OUTDIR=./output
if [ -n "$1" ]; then
    OUTDIR="${OUTDIR}/$1"
fi

TARGET=infer

docker run --rm -it \
    -v ${SCRIPT_DIR}/.inferconfig:/workspace/.inferconfig:ro \
    -v ${SCRIPT_DIR}/analysis.py:/workspace/juliet/analysis.py:ro \
    -v ${OUTDIR}/${TARGET}:/workspace/output \
    -e RESULTS_DIR=/workspace/output \
    --network none \
   juliet-java-${TARGET} ./analysis.py
popd