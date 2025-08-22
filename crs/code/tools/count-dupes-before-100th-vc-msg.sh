#!/bin/bash

# Usage: ./script.sh input.txt

input="${1:-/dev/stdin}"
count=0

awk '
/Got vuln-cand-msg \(600\)/ {exit}
 /Dropping vc which matches/ {count++}
 END {print count}
' "$input"
