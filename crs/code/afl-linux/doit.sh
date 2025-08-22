#!/usr/bin/env bash
set -euxo pipefail

# This script should be run from inside the VM.
test "$(hostname)" = afl-linux

# Go to the host afl-linux directory.
cd /code/afl-linux

# Make the fuzzer directories, if they don't exist.
mkdir -p /tmp/fuzz-in /tmp/fuzz-out

# Make our example TIPC packet, if it doesn't exist.
test -e /tmp/fuzz-in/tipc-packet || xxd -r >/tmp/fuzz-in/tipc-packet <<EOF
00000000: 8e1b 1351 22fd
00000006: 1234 5678 9abc
0000000c: 88ca
0000000e: 5740
00000010: 0000 0000 0000 0000 0000 0000 0000 0000
00000020: 0000 0000 0000 0000 0000 0000 0000 0000
00000030: 0000 0000 0000 0000 0000 0000 0000 0000
00000040: 0000 0000 0000 0000 0000 0000 0000 0000
EOF

# Run the harness.
afl-fuzz \
	-i /tmp/fuzz-in \
	-o /tmp/fuzz-out \
	-s 0 \
	-- \
	harness/target/x86_64-unknown-linux-musl/release/afl-linux-harness \
	--guidance-config test-guidance.json \
	test-guidance
