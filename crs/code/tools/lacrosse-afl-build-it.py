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
arg_parser = ArgumentParser(prog="lacrosse-afl-build-it.py")
args = arg_parser.parse_args()

# Wait until AFL++, LLVM, etc. are copied over.
while not Path("/crs_scratch/sift-container-stuff/nix-copy-done").exists():
    print("waiting for nix-copy...", file=stderr)
    sleep(5)

# Do test-compiles to get the default cc1 and ld calls.
@dataclass
class CompilerInfo:
    include: list[str]
    isystem: list[str]
    internal_isystem: list[str]
    internal_externc_isystem: list[str]
    linker: list[str]

    def cflags(self) -> list[str]:
        out = ["-nostdinc", "--ld-path=" + str(AFLPP / "bin/sift-ld.lld"), "-g"]
        for path in self.include:
            out.append(f"-I{path}")
        for path in self.isystem:
            out.append("-Xclang")
            out.append("-isystem")
            out.append("-Xclang")
            out.append(path)
        for path in self.internal_isystem:
            out.append("-Xclang")
            out.append("-internal-isystem")
            out.append("-Xclang")
            out.append(path)
        for path in self.internal_externc_isystem:
            out.append("-Xclang")
            out.append("-internal-externc-isystem")
            out.append("-Xclang")
            out.append(path)
        return out

    def ldflags(self) -> list[str]:
        out = []
        for path in self.linker:
            out.append(f"-L{path}")
            out.append("-rpath")
            out.append(path)
        return out

def test_compile(compiler: str, ext: str) -> CompilerInfo:
    test_file = Path(f"work/sift_test_compile.{ext}")
    test_file.touch()
    output = run([
        "bash",
        "run.sh",
        "-x",
        "custom",
        compiler,
        "-###",
        Path("/") / test_file,
    ], check=True, stdout=DEVNULL, stderr=PIPE).stderr
    
    # Remove a terminating newline, then split out the last two lines.
    if output.endswith(b"\n"):
        output = output[:-1]
    _, cc1_line, ld_line = output.rsplit(b"\n", 2)
    print(f"output = {output!r}")
    print(f"cc1_line = {cc1_line!r}")
    print(f"ld_line = {ld_line!r}")

    # Prepare the output object.
    out = CompilerInfo(include=[], isystem=[], internal_isystem=[], internal_externc_isystem=[], linker=[])

    # Parse the compiler and linker args.
    on_next = None
    for arg in shlex.split(cc1_line.decode("utf-8")):
        if on_next is not None:
            on_next(arg)
            on_next = None
        if arg == "-I":
            on_next = lambda arg: out.include.append(arg)
        elif arg.startswith("-I"):
            out.include.append(arg[2:])
        elif arg == "-isystem":
            on_next = lambda arg: out.isystem.append(arg)
        elif arg == "-internal-isystem":
            on_next = lambda arg: out.internal_isystem.append(arg)
        elif arg == "-internal-externc-isystem":
            on_next = lambda arg: out.internal_externc_isystem.append(arg)
    for arg in shlex.split(ld_line.decode("utf-8")):
        if on_next is not None:
            on_next(arg)
            on_next = None
        if arg == "-L":
            on_next = lambda arg: out.linker.append(arg)
        elif arg.startswith("-L"):
            out.linker.append(arg[2:])
    return out
cc_info = test_compile("clang", "c")
cxx_info = test_compile("clang++", "cc")

# Come up with our environment.
our_envs = {
    "CC": str(AFLPP / "bin/afl-cc"),
    "CXX": str(AFLPP / "bin/afl-c++"),
    "CP_HARNESS_EXTRA_CFLAGS": shlex.join(cc_info.cflags()),
    "CP_HARNESS_EXTRA_CXXFLAGS": shlex.join(cxx_info.cflags()),
    "CP_HARNESS_EXTRA_LDFLAGS": shlex.join(cxx_info.ldflags()),
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
print("cc_info =", cc_info)
print("cxx_info =", cxx_info)

# Run the build.
run(["bash", "run.sh", "-x", "build"], check=True, env=env)
