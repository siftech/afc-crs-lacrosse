#!/usr/bin/env bash

PROGNAME=$(basename "$0")
warn()  { echo "$PROGNAME: $*" 1>&2; }
die()   { warn "${@}"; exit 1; }
dbug()   { test -z "$DEBUG" || warn "${@}"; }

PROJECT_DIR=${1}
# dbug "Project dir is $PROJECT_DIR"

lines_of_c=$(find "$PROJECT_DIR" \( -name "*.c" -o -name "*.h" \) -type f -print0 | xargs -0 cat 2>/dev/null | wc -l)
# dbug "Lines of C code: $lines_of_c"

lines_of_java=$(find "$PROJECT_DIR" -name "*.java" -type f -print0 | xargs -0 cat 2>/dev/null | wc -l)
# dbug "Lines of Java code: $lines_of_java"

if (( lines_of_c < lines_of_java )); then
    echo "Java"
else
    echo "C"
fi
