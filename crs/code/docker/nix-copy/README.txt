= nix

This directory contains a Nix flake that depends on all the packages we'll be
mounting into the containers created by various CP's `run.sh` scripts. We're
using Nix to avoid having to build our own tools to a custom `$PREFIX`, on the
assumption that the actual CPs won't need it.

// vim: set ft=typst :
