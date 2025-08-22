#!/usr/bin/env python3

from argparse import ArgumentParser
from dataclasses import dataclass
import json
from os import environ
from pathlib import Path
import shlex
from subprocess import check_output, run, DEVNULL, PIPE
from sys import stderr
from time import sleep

# The location of our AFL++ stuff, inside the container created by run.sh.
AFLPP = Path("/sift-container-stuff/aflplusplus")

# Parse the arguments.
arg_parser = ArgumentParser(prog="lacrosse-afl-fuzz-it.py")
arg_parser.add_argument("harness_id")
arg_parser.add_argument("corpus_dir")
arg_parser.add_argument("fuzz_out_dir", nargs="?")
arg_parser.add_argument("timeout")
arg_parser.add_argument("fuzzer_args", nargs="*")
args = arg_parser.parse_args()

# Check that the corpus directory exists.
corpus_dir = Path(args.corpus_dir or f"work/sift_afl_{args.harness_id}_corpus")
print(f"Using a fuzzer input directory of {corpus_dir}...", file=stderr)
if not corpus_dir.exists():
    print(f"fuzzer input directory {corpus_dir} did not exist", file=stderr)
    exit(1)

# Check that the fuzzer output directory exists.
fuzz_out_dir = Path(args.fuzz_out_dir or f"work/sift_afl_{args.harness_id}_fuzz")
fuzz_out_dir.mkdir(parents=True, exist_ok=True)
print(f"Using a fuzzer output directory of {fuzz_out_dir}...", file=stderr)

# Parse the project.yaml.
project_yaml = json.loads(check_output(["yq", "eval", "-o", "json", ".", "project.yaml"]))
if not args.harness_id in project_yaml["harnesses"]:
    print(f"harness {args.harness_id!r} did not exist", file=stderr)
    exit(1)
harness_bin = Path(project_yaml["harnesses"][args.harness_id]["binary"])

# Come up with our environment.
our_envs = {
    "AFL_AUTORESUME": "1",
    "AFL_IGNORE_SEED_PROBLEMS": "1",
    "AFL_IMPORT_FIRST": "1",
    "AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES": "1",
    "AFL_PIZZA_MODE": "-1",
    "AFL_SKIP_CPUFREQ": "1",
    "ASAN_OPTIONS": "abort_on_error=1 detect_leaks=0 symbolize=0",
}

# Come up with the Docker arguments.
docker_extra_args = [
    "-v", "/crs_scratch:/crs_scratch",
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

# Write out the harness.
afl_harness_name = f"sift_afl_{args.harness_id}.sh"
afl_harness_path = Path(f"out/{afl_harness_name}")
with afl_harness_path.open("w") as f:
    f.write(f"""#!/bin/sh
set -eux
timeout --verbose {args.timeout} {AFLPP / "bin/afl-fuzz"} \\
    -i {corpus_dir} \\
    -o {fuzz_out_dir} \\
    {shlex.join(args.fuzzer_args)} \\
    -- {Path("/") / harness_bin}
""")
afl_harness_path.chmod(0o777)

# Run the fuzzer.
dummy_file = Path("work/sift_afl_empty")
dummy_file.touch()
run(["bash", "run.sh", "-x", "run_pov", dummy_file, afl_harness_name], check=True, env=env)
