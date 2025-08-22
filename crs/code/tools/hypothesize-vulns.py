#!/usr/bin/env python3

# A script entrypoint for the finals lax agent to hypothesize vulnerabilities via dspy

DEFAULT_MAX_RETRIES = 3

# Telemetry
import openlit
openlit.init(
    application_name="lacrosse-hypothisize-vulns",
)

import ast
import os
import sys
import re
import argparse
import importlib
import subprocess
import shutil

tool_dir = os.path.dirname(os.path.abspath(__file__))
# This is probably the reason all our imports work in the dspy code (but also below).
dspy_path = os.path.join(tool_dir, "..", "dspy")
print("dspy path is", dspy_path)
if dspy_path not in sys.path:
    sys.path.append(dspy_path)

from models import pipeline_model, find_model, try_models
from record import records_directory, record
from utils import read_file, write_to_file, count_files_and_dirs, clean_fuzzing_output, attempt_n_times
from crash import extract_stderr, strip_ansi, build_and_reproduce, can_reproduce

# Set up the argument parser
# For round 1 only the llms, delta, sani, and blob args will be supported
parser = argparse.ArgumentParser(description="Process two required arguments and five optional keyword arguments: pipeline, llms, delta, sani, blob, harn.")

# Required positional arguments
parser.add_argument("codebase_path", help="The path to the codebase to be patched.")
parser.add_argument("vulns_path", help="The path to the vulnerability hypotheses to be produced.")

# Optional keyword arguments
parser.add_argument("-pipeline", "--pipeline", help="The name of the pipeline to be used", default="hypothesize_vulns")
parser.add_argument("-scratch", "--scratch_path", help="Path to dir for storing intermediate outputs and logs of patch generation", default="gen-finals-patch-scratch")
parser.add_argument("-llms", "--llms", nargs="+", help="A list of llm names to use, in the order we want to resort to them in (on failure)", default="sonnet37 gpt4o")
parser.add_argument("-delta", "--delta_path", help="Path to delta (diff)", default=None)
parser.add_argument("-sani", "--sanitizer_stderr_path", help="Path to sanitizer stderr log for pov", default=None)
parser.add_argument("-blob", "--blob_path", help="Path to pov blob, i.e. test case", default=None)
parser.add_argument("-harn", "--harness_name", help="Name of harness for pov", default=None)
parser.add_argument("-psblob", "--pass_blob", help="Bool to say whether to provide blob to LLM to help hypothesize vulnerabilities", default=True)
parser.add_argument("-psharn", "--pass_harness", help="Bool to say whether to provide harness to LLM to help hypothesize vulnerabilities", default=False)
parser.add_argument("-retries", "--retries", help="The max retries through the pipeline allowed", default=DEFAULT_MAX_RETRIES)

# Parse arguments
args = parser.parse_args()

# Print parsed values
print("hypothesize-vulns.py received args:")
print(f"codebase_path: {args.codebase_path}")
print(f"vulns_path: {args.vulns_path}")
print(f"pipeline (-pipeline): {args.pipeline}")
print(f"scratch_path (-scratch): {args.scratch_path}")
print(f"llms (-llms): {args.llms}")
print(f"delta_path (-delta): {args.delta_path}")
print(f"sanitizer_stderr_path (-sani): {args.sanitizer_stderr_path}")
print(f"blob_path (-blob): {args.blob_path}")
print(f"harness_name (-harn): {args.harness_name}")
print(f"pass_blob (-psblob): {args.pass_blob}")
print(f"pass_harness (-psharn): {args.pass_harness}")
print(f"retries (-retries): {args.retries}")


llms = args.llms
models = [find_model(llm) for llm in llms]

delta = None
if args.delta_path is not None:
    delta = read_file(args.delta_path)

sanitizer_stderr = None
if args.sanitizer_stderr_path is not None:
    sanitizer_stderr = read_file(args.sanitizer_stderr_path)

pov_blob = None
if args.blob_path is not None:
    pov_blob = read_file(args.blob_path)

pov_harness = None
if args.harness_name is not None:
    harness_path = None # FIXME If there's a way to get this from the harness name, circle back and fill this in
    if harness_path is not None:
        pov_harness = read_file(harness_path)

n_attempts = int(args.retries) + 1
        
def attempt_pipeline(i):
    attempt_scratch_path = os.path.join(args.scratch_path, f"attempt{i}")
    with records_directory(attempt_scratch_path):
        # Actually run the pipeline and generate the patch (as a string)

        codebase_contents_count = count_files_and_dirs(args.codebase_path)
        record("pipeline_inputs/codebase_contents", codebase_contents_count, True)
        record("pipeline_inputs/llms", llms, True)
    
        try:
            pipeline_module = importlib.import_module(f"pipelines.{args.pipeline}")
            vulns = try_models(models, pipeline_module.hypothesize_vulns,
                               args.codebase_path,
                               sanitizer_stderr,
                               delta,
                               pov_blob if args.pass_blob else None,
                               pov_harness if args.pass_harness else None)
        except ImportError:
            raise ValueError(f"Module {args.pipeline} not found. We expect there to be a module named the same thing as the pipeline.")
        except AttributeError:
            raise ValueError(f"Module {args.pipeline} does not have a 'hypothesize_vulns' function. We expect it to.")
        except Exception as e:
            raise e

        record(f"hypothesized_vulns", json.dumps(vulns, indent=2), False)

        for llm_name, model in zip(llms, models):
            history = model.history
            record(f"{llm_name}_history", str(history), False)

        return vulns

# Ensure the sanitizer stderr is cleaned
if sanitizer_stderr is not None:
    sanitizer_stderr = clean_fuzzing_output(sanitizer_stderr)
    
# FIXME Right now this is just retrying if a null or empty patch gets generated,
# but it might be able to be extended to do the testing.

def assert_good_vulns (vulns):
    assert vulns is not None, "The answer is None"
    assert len(vulns) > 0, "The vulns have length 0"

final_vulns = attempt_n_times(attempt_pipeline, n_attempts, assert_good_vulns, "Hypothesizing vulnerabilities")

write_to_file(args.vulns_path, json.dumps(final_vulns, indent=2))

# For sanity checking
final_vulns_contents = read_file(args.vulns_path)
print()
print("Contents of final vulns:")
print(final_vulns_contents)
print()

print(f"Wrote final vulns to {args.vulns_path}")

