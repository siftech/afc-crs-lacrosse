#!/usr/bin/env python3

import sys

# takes two arguments, a file with lines of regular text, and an output file
# converts each line in the input file an escaped format with each character as an escaped
# hexadecimal value surrounded in quotes, for the libfuzzer dictionary.

file_in = sys.argv[1]
file_out = sys.argv[2]

with open(file_in,"rb") as f_in:
    with open(file_out,"w") as f_out:
        for line in f_in:
            hexxed = line.rstrip(b"\n").hex().upper()
            f_out.write("\"\\x" + "\\x".join(hexxed[i:i+2] for i in range(0, len(hexxed), 2)) + "\"\n")
