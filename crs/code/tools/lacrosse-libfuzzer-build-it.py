#!/usr/bin/env python3

from argparse import ArgumentParser
from dataclasses import dataclass
from os import environ
from pathlib import Path
import shlex
from subprocess import run, DEVNULL, PIPE
from sys import stderr
from time import sleep

# The location of our AFL++ stuff, inside the container created by run.sh.
AFLPP = Path("/sift-container-stuff/aflplusplus")

# Parse the arguments.
arg_parser = ArgumentParser(prog="lacrosse-libfuzzer-build-it.py")
args = arg_parser.parse_args()

# Wait until AFL++, LLVM, etc. are copied over.
while not Path("/crs_scratch/sift-container-stuff/nix-copy-done").exists():
    print("waiting for nix-copy...", file=stderr)
    sleep(5)

# Come up with our environment.
cflags = ["-g"]
cxxflags = ["-g"]
ldflags = []
our_envs = {
    "CC": str(AFLPP / "bin/sift-clang"),
    "CXX": str(AFLPP / "bin/sift-clang++"),
    "CP_HARNESS_EXTRA_CFLAGS": shlex.join(cflags),
    "CP_HARNESS_EXTRA_CXXFLAGS": shlex.join(cxxflags),
    "CP_HARNESS_EXTRA_LDFLAGS": shlex.join(ldflags),
}
our_envs["CCC"] = our_envs["CXX"]
our_envs["CP_BASE_EXTRA_CFLAGS"] = our_envs["CP_HARNESS_EXTRA_CFLAGS"]
our_envs["CP_BASE_EXTRA_CXXFLAGS"] = our_envs["CP_HARNESS_EXTRA_CXXFLAGS"]
our_envs["CP_BASE_EXTRA_LDFLAGS"] = our_envs["CP_HARNESS_EXTRA_LDFLAGS"]

# Come up with the Docker arguments.
docker_extra_args = [
    "-v", "/crs_scratch/nix:/nix",
    "-v", "/crs_scratch/sift-container-stuff:/sift-container-stuff",
]
for name in our_envs:
    docker_extra_args.append("-e")
    docker_extra_args.append(name)

# Come up with the final environment to use.
env = dict(environ)
env["DOCKER_EXTRA_ARGS"] = shlex.join(docker_extra_args)
for k, v in our_envs.items():
    env[k] = v

# Log what we're about to do.
print("About to build with:")
print("DOCKER_EXTRA_ARGS=" + shlex.join(docker_extra_args))
for k, v in our_envs.items():
    print(f"{k}={v}")

# Run the build.
run(["bash", "run.sh", "-x", "build"], check=True, env=env)
