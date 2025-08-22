#!/usr/bin/env python3

from argparse import ArgumentParser
import json
from os import environ
from pathlib import Path
import shlex
from subprocess import check_output, run
from sys import stderr

# The location of our AFL++ stuff, inside the container created by run.sh.
AFLPP = Path("/sift-container-stuff/aflplusplus")

# Parse the arguments.
arg_parser = ArgumentParser(prog="lacrosse-libfuzzer-fuzz-it.py")
arg_parser.add_argument("harness_id")
arg_parser.add_argument("corpus_dir")
arg_parser.add_argument("fuzz_dir", nargs="?")
arg_parser.add_argument("timeout")
arg_parser.add_argument("fuzzer_args", nargs="*")
args = arg_parser.parse_args()

# Check that the corpus directory exists.
corpus_dir = Path(args.corpus_dir or f"work/sift_afl_{args.harness_id}_corpus")
corpus_dir.mkdir(parents=True, exist_ok=True)
print(f"Using a fuzzer input directory of {corpus_dir}...", file=stderr)
if not corpus_dir.exists():
    print(f"fuzzer input directory {corpus_dir} did not exist", file=stderr)
    exit(1)

# Check that the fuzzer directory exists.
fuzz_dir = Path(args.fuzz_dir or f"work/sift_libfuzzer_{args.harness_id}_fuzz")
fuzz_dir.mkdir(parents=True, exist_ok=True)
print(f"Using a fuzzer output directory of {fuzz_dir}...", file=stderr)

# Parse the project.yaml.
project_yaml = json.loads(check_output(["yq", "eval", "-o", "json", ".", "project.yaml"]))
if not args.harness_id in project_yaml["harnesses"]:
    print(f"harness {args.harness_id!r} did not exist", file=stderr)
    exit(1)
harness_bin = Path(project_yaml["harnesses"][args.harness_id]["binary"])

# Come up with our args.
fuzzer_args = list(args.fuzzer_args)

# Come up with the Docker arguments.
docker_extra_args = [
    "-v", "/crs_scratch:/crs_scratch",
    "-v", "/crs_scratch/nix:/nix",
    "-v", "/crs_scratch/sift-container-stuff:/sift-container-stuff",
]

# Come up with the final environment to use.
env = dict(environ)
env["DOCKER_EXTRA_ARGS"] = shlex.join(docker_extra_args)

# Write out the harness.
harness_wrapper_name = f"sift_libfuzzer_{args.harness_id}.sh"
harness_wrapper_path = Path(f"out/{harness_wrapper_name}")
with harness_wrapper_path.open("w") as f:
    f.write(f"""#!/bin/sh
set -eux
# Don't think about it... accept life's mysteries...
timeout --verbose {args.timeout} \\
  {AFLPP / "bin/loopwhilex"} \\
  {AFLPP / "bin/foreground"} \\
    {shlex.quote(" " + str(Path("/") / harness_bin))} \\
    {shlex.quote(" " + str(Path("/") / fuzz_dir))}    \\
    {shlex.quote(" " + str(Path("/") / corpus_dir))}  \\
    /crs_scratch/canned-corpus-copy/                  \\
    {shlex.join(" " + arg for arg in fuzzer_args)}    \\
  "" \\
  echo resuming fuzzing...
# echo above needed for loopwhilex to get an exit code 0
""")
harness_wrapper_path.chmod(0o777)

# Run the fuzzer.
dummy_file = Path("work/sift_libfuzzer_empty")
dummy_file.touch()
run(["bash", "run.sh", "-x", "run_pov", dummy_file, harness_wrapper_name], check=True, env=env)
