#!/bin/bash
export PATH="/home/lax/bin:$PATH"
exec /home/lax/bin/dockerd-rootless.sh "$@"
