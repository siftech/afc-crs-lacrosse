#!/usr/bin/env bash
set -euxo pipefail

test -d /crs_scratch
mkdir -p /crs_scratch/sift-container-stuff
rsync -a --delete /nix/ /crs_scratch/nix/
rsync -a --delete /lacrosse/aflplusplus /crs_scratch/sift-container-stuff/
# TODO: Startup checks go here

touch /crs_scratch/sift-container-stuff/nix-copy-done
