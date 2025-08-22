#!/usr/bin/env bash
set -euo pipefail

file="${1:-OPTIMUS0.log}"

cd "$LACROSSE_HOME/code/test/results"
dir="$(ls -1t | head -1)"
cd "$dir"
if [[ ! -f "$file" ]]; then
	printf "waiting for $file"
	while [[ ! -f "$file" ]]; do
		sleep 1
		printf .
	done
	printf "\a\n"
fi
tail -f "$file"
