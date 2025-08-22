#!/usr/bin/env bash

# Run in log (results) dir to see most recent state of task queue for all fuzzbombs
for log in $(ls FUZZBOMB* | sort -V); do
    echo "---------- $log ----------"
    grep "Selected #<" $log | tail -1
done
