#!/bin/bash

# SIFT-Specific fuzzer script that goes in container

set -ex
set -o pipefail

warn() {
    echo "$*" >&2
}

die() {
    warn "$*"
    exit 1
}

ran_successfully() {
  exit 0
}

HARNESS_NAME=${1:-}
SOURCE_VAL=${2:-}
BINARY_PATH=${3:-}
DICT_FILE=${4:-}
SEEDS_DIR=${5:-}
OUT_PATH=${6:-}
TIMEOUT=${7:-}
shift 7

# they better be providing us only *.java paths in source...
SOURCE_PATH_BASE="${SOURCE_PATH##*/}"
SOURCE_PATH_BASE_NO_EXTN="${SOURCE_PATH_BASE%.java}"

[[ -n "$HARNESS_NAME" ]] || die "Invalid input, missing harness: $HARNESS_NAME"
[[ -d ${SRC}/maven_repo ]] || die "Missing dependencies folder: ${SRC}/maven_repo"

# hope this too is a jar...
HARNESS_JAR="/${BINARY_PATH}"
BINARY_PATH_DIR=$(dirname "$HARNESS_JAR")

# TODO: multiple class names here?
FULL_CLASS_NAME=$(jar tf "/${BINARY_PATH}" | grep ${SOURCE_PATH_BASE_NO_EXTN}.class | sed 's/\//./g')
FULL_CLASS_NAME=${FULL_CLASS_NAME%.class}

echo "FULL_CLASS_NAME=${FULL_CLASS_NAME}"

#jazzer doesn't appear to like wildcarding jars. We have to do this work-around
HARNESS_CP=$(find ${BINARY_PATH_DIR} -name '*.jar' -printf '%p:' | sed 's/:$//')

echo "HARNESS_NAME=${HARNESS_NAME} \
  HARNESS_CLASSNAME=${FULL_CLASS_NAME}"


# Construct a dictionary file from every class file in the jars
# TODO: Filter this to focus on the jars of interest
# DICT_FILE="${OUT_PATH}/jazzer-dictionary"
# rm -rf ${DICT_FILE}
# touch ${DICT_FILE}

# HARNESS_CP_JARS=$(find ${BINARY_PATH_DIR} -name '*.jar' -printf '%p:' | sed 's/:$//')
# find ${BINARY_PATH_DIR} -name '*.jar' | while read -r JAR_FILE; do
#     unzip -jp "$JAR_FILE" '*class' | strings -a | sort -u | sed 's/.*/"&"/' >> ${DICT_FILE}
# done

# TEMP_DIR=/out/foo
# CUR_DIR=$(pwd)
# cd ${TEMP_DIR}
# HARNESS_CP_JARS=$(find ${BINARY_PATH_DIR} -name '*.jar' -printf '%p:' | sed 's/:$//')
# find ${BINARY_PATH_DIR} -name '*.jar' | while read -r JAR_FILE; do
#     echo $JAR_FILE
#     # Extract .class files from the JAR and run strings on each one
#     jar tf "$JAR_FILE" | grep '\.class$' | while read -r CLASS_FILE; do
#         jar xf "$JAR_FILE" "$CLASS_FILE" && strings -a "${TEMP_DIR}/${CLASS_FILE}" | sort -u | sed 's/.*/"&"/' >> ${DICT_FILE}
#     done
# done

# cd $CUR_DIR
# rm -rf ${TEMP_DIR}


exit_code=0





echo "OUT_PATH=${OUT_PATH}"
echo "SEEDS_DIR=${SEEDS_DIR}"
timeout --verbose --signal=KILL "$TIMEOUT" /classpath/jazzer/jazzer \
  --agent_path=/classpath/jazzer/jazzer_standalone_deploy.jar \
  "--cp=${HARNESS_CP}" \
  "--keep_going=10" \
  "--target_class=${FULL_CLASS_NAME}" \
  "--jvm_args=-Djdk.attach.allowAttachSelf=true:-XX\:+StartAttachListener" \
  "$@" \
  "-workers=1" \
  "-dict=${DICT_FILE}" \
  "-artifact_prefix=${OUT_PATH}" \
  "${OUT_PATH}" \
  "${SEEDS_DIR}" \
  /crs_scratch/canned-corpus-copy/ \
  || exit_code=$?

echo "jazzer exit=${exit_code}"

if [ $exit_code -eq 0 ]; then #not triggered
  ran_successfully
elif [ $exit_code -eq 70 ]; then #timeout
  die "timeout"
elif [ $exit_code -eq 124 ]; then #timeout
  die "timeout"
elif [ $exit_code -eq 76 ]; then #triggered asan
  ran_successfully
elif [ $exit_code -eq 77 ]; then #triggered a sanitizer or could be a java exception, oom, npe, etc
  ran_successfully
else
  die "jazzer crashed with an unexpected exit code"
fi
