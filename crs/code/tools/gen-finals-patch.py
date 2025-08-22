#!/usr/bin/env python3

DEFAULT_MAX_RETRIES = 1

# A script entrypoint for the finals lax agent to generate patches via dspy


# Telemetry
import openlit
openlit.init(
    application_name="lacrosse",
)

import ast
import os
import sys
import re
import argparse
import importlib
import subprocess

tool_dir = os.path.dirname(os.path.abspath(__file__))
# This is probably the reason all our imports work in the dspy code (but also below).
dspy_path = os.path.join(tool_dir, "..", "dspy")
print("dspy path is", dspy_path)
if dspy_path not in sys.path:
    sys.path.append(dspy_path)

from models import pipeline_model, find_model, try_models
from record import records_directory, record
from utils import read_file, write_to_file, count_files_and_dirs, clean_fuzzing_output, attempt_n_times
from crash import test_w_feedback, can_reproduce


# Set up the argument parser
# For round 1 only the llms, delta, sani, and blob args will be supported
parser = argparse.ArgumentParser(description="Process two required arguments and five optional keyword arguments: pipeline, llms, delta, sani, blob, harn.")

# Required positional arguments
parser.add_argument("codebase_path", help="The path to the codebase to be patched.")
parser.add_argument("patch_path", help="The path to the patch to be produced.")

# Optional keyword arguments
parser.add_argument("-pipeline", "--pipeline", help="The name of the pipeline to be used", default="discover_then_patch_vulns")
parser.add_argument("-scratch", "--scratch_path", help="Path to dir for storing intermediate outputs and logs of patch generation", default="gen-finals-patch-scratch")
parser.add_argument("-llms", "--llms", nargs="+", help="A list of llm names to use, in the order we want to resort to them in (on failure)", default="sonnet37 gpt4o")
parser.add_argument("-delta", "--delta_path", help="Path to delta (diff)", default=None)
parser.add_argument("-sani", "--sanitizer_stderr_path", help="Path to sanitizer stderr log for pov", default=None)
parser.add_argument("-blob", "--blob_path", help="Path to pov blob, i.e. test case", default=None)
parser.add_argument("-harn", "--harness_name", help="Name of harness for pov", default=None)
parser.add_argument("-saniname", "--sanitizer_name", help="Name of the sanitizer, for testing the patch against the pov.", default=None)
parser.add_argument("-fuzzeng", "--fuzzing_engine", help="Name of the fuzzing engine, for testing the patch against the pov.", default=None)
parser.add_argument("-proj", "--project_name", help="The project name, for testing the patch against the pov.", default=None)
parser.add_argument("-psblob", "--pass_blob", help="Bool to say whether to provide blob to LLM for patching", default=True)
parser.add_argument("-psharn", "--pass_harness", help="Bool to say whether to provide harness to LLM for patching", default=False)
parser.add_argument("-retries", "--retries", help="The max retries through the pipeline allowed", default=DEFAULT_MAX_RETRIES)

# Parse arguments
args = parser.parse_args()

# Print parsed values
print("gen-finals-patch.py received args:")
print(f"codebase_path: {args.codebase_path}")
print(f"patch_path: {args.patch_path}")
print(f"pipeline (-pipeline): {args.pipeline}")
print(f"scratch_path (-scratch): {args.scratch_path}")
print(f"llms (-llms): {args.llms}")
print(f"delta_path (-delta): {args.delta_path}")
print(f"sanitizer_stderr_path (-sani): {args.sanitizer_stderr_path}")
print(f"blob_path (-blob): {args.blob_path}")
print(f"harness_name (-harn): {args.harness_name}")
print(f"sanitizer_name (-saniname): {args.sanitizer_name}")
print(f"fuzzing_engine (-fuzzeng): {args.fuzzing_engine}")
print(f"project_name (-proj): {args.project_name}")
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
    pov_blob = read_file(args.blob_path, err_ok=True)

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
            patch = try_models(models, pipeline_module.generate_patch,
                               args.codebase_path,
                               sanitizer_stderr,
                               delta,
                               pov_blob if args.pass_blob else None,
                               pov_harness if args.pass_harness else None)
        except ImportError:
            raise ValueError(f"Module {args.pipeline} not found. We expect there to be a module named the same thing as the pipeline.")
        except AttributeError:
            raise ValueError(f"Module {args.pipeline} does not have a 'generate_patch' function. We expect it to.")
        except Exception as e:
            raise e

        record(f"generated_patch", patch, False)

        for llm_name, model in zip(llms, models):
            history = model.history
            record(f"{llm_name}_history", str(history), False)

        return patch

# Got rid of the stuff that reproduces a stderr if one is missing.
# Bad bc of code duplication, and it masks comms issues between tasks in the lisp.
        
# Ensure the sanitizer stderr is cleaned
if sanitizer_stderr is not None:
    sanitizer_stderr = clean_fuzzing_output(sanitizer_stderr)

def assert_good_patch (patch, attempt):
    assert patch is not None, "The answer is None"
    assert len(patch) > 0, "The patch has length 0"
    if can_reproduce(args):
        print(f"Test patch for attempt {attempt}.")
        attempt_scratch_path = f"{args.scratch_path}/attempt{attempt}"
        passes, feedback = test_w_feedback(patch, attempt_scratch_path, args.blob_path, args.harness_name, args.codebase_path, args.sanitizer_name, args.fuzzing_engine, args.project_name)
        assert passes, "The patch did not pass the pov test case!"
    else:
        print("Skip testing patch against pov. Insufficient inputs for testing.")

# FIXME Right now this is just retrying if a null or empty patch gets generated,
# But it might be able to be extended to do the testing
final_patch = attempt_n_times(attempt_pipeline, n_attempts, assert_good_patch, "Patch generation")

write_to_file(args.patch_path, final_patch)

# For sanity checking
final_patch_contents = read_file(args.patch_path)
print()
print("Contents of final patch:")
print(final_patch_contents)
print()

print(f"Wrote final patch to {args.patch_path}")
