#!/usr/bin/env sh
set -ex
# get directory containing the script
SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE[0]}")")"
SCRIPT_FILE="$(basename "${BASH_SOURCE[0]}")"


HARNESS_NAME=${1:-}
SOURCE_PATH=${2:-}
BINARY_PATH=${3:-}
SEEDS_DIR=${4:-}
OUT_DIR=${5:-}
TIMEOUT=${6:-}
shift 6

mkdir -p ${OUT_DIR}

export DOCKER_EXTRA_ARGS="${DOCKER_EXTRA_ARGS} -v /crs_scratch:/crs_scratch"

# Should be put here by a call to make-string-dictionary.sh in the lisp
DICT_FILE=/work/strings.dict

cp $SCRIPT_DIR/../crs-sandbox-items/jazzer-fuzz.sh out

./run.sh -x custom /out/jazzer-fuzz.sh $HARNESS_NAME "/$SOURCE_PATH" "/$BINARY_PATH" "$DICT_FILE" "/$SEEDS_DIR" "/$OUT_DIR/" "$TIMEOUT" "$@"
