#!/usr/bin/env bash

# Run in log (results) dir to see most recent state of task queue for all fuzzbombs
for log in $(ls FUZZBOMB* | sort -V); do
    echo "----------------------------------------------------------------------"
    echo $log
    grep -A 5 "Task queue" $log | tail -6
done
