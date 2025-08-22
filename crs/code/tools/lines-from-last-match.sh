#!/bin/bash

# Usage: echo "input text" | ./script.sh <pattern>
# Example: cat input.txt | ./script.sh "PATTERN"

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <pattern>"
    exit 1
fi

pattern=$1

awk -v pat="$pattern" '
  { if ($0 ~ pat) last_match = NR }
  END { if (last_match) for (i = last_match; i <= NR; i++) print lines[i] }
  { lines[NR] = $0 }
' <&0
