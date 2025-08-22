#!/usr/bin/env python3

DEFAULT_MAX_RETRIES = 3

# Note: Outer retries start from scratch and do not run tests, unlile the inner-loop
# retries (above) which are the primary mechanism.
OUTER_RETRIES = 0

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
import json
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
from record import records_directory, record, get_records_directory
from custom_types import Vulnerability
from utils import read_file, write_to_file, count_files_and_dirs, attempt_n_times
from crash import test_w_feedback
from parse_reproduce import parse_sanitizer_stderr_with_instrumentation, clean_fuzzing_output

# Set up the argument parser
# For round 1 only the llms, delta, sani, and blob args will be supported
parser = argparse.ArgumentParser(description="Process three required arguments and five optional keyword arguments: pipeline, llms, delta, sani, blob, harn.")

# Required positional arguments
parser.add_argument("hyps_json_path", help="The path to the json file storing vulnerabilties to be patched.")
parser.add_argument("codebase_path", help="The path to the codebase to be patched.")
parser.add_argument("patch_path", help="The path to the patch to be produced.")

# Optional keyword arguments
parser.add_argument("-pipeline", "--pipeline", help="The name of the pipeline to be used", default="patch_vulns_w_feedback")
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
parser.add_argument("-reqpass", "--require_tests_pass", help="Bool to say whether to err if not patch is found that passes all tests", default=True)

# Parse arguments
args = parser.parse_args()

# Print parsed values
print("patch-vulns.py received args:")
print(f"hyps_json_path: {args.hyps_json_path}")
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

with open(args.hyps_json_path) as f:
    vulns_data = json.load(f)
    vulns = []
    for i, vuln_dict in enumerate(vulns_data):
        print(f"vuln_dict({i})=")
        print(json.dumps(vuln_dict, indent=2))
        vuln = Vulnerability.model_validate(vuln_dict)
        vulns.append(vuln)

delta = None
if args.delta_path is not None:
    delta = read_file(args.delta_path)

sanitizer_stderr = None
if args.sanitizer_stderr_path is not None:
    sanitizer_stderr = read_file(args.sanitizer_stderr_path)
    with records_directory(args.scratch_path):
        sanitizer_stderr = parse_sanitizer_stderr_with_instrumentation(sanitizer_stderr, args.project_name)

pov_blob = None
if args.blob_path is not None:
    pov_blob = read_file(args.blob_path, err_ok=True)

pov_harness = None
if args.harness_name is not None:
    harness_path = None # FIXME If there's a way to get this from the harness name, circle back and fill this in
    if harness_path is not None:
        pov_harness = read_file(harness_path)    

n_attempts = int(args.retries) + 1


# Ensure the sanitizer stderr is cleaned
if sanitizer_stderr is not None:
    sanitizer_stderr = clean_fuzzing_output(sanitizer_stderr)

def validate_patch(patch, val_dir):
    print("validate_patch", type(patch), len(patch), type(val_dir), val_dir)
    assert patch is not None, "The answer is None"
    assert len(patch) > 0, "The patch has length 0"
    records_dir = get_records_directory()
    print("records_dir=", records_dir)
    passes, feedback, score = test_w_feedback(patch, records_dir, args.blob_path, args.harness_name, args.codebase_path, args.sanitizer_name, args.fuzzing_engine, args.project_name)
    return (passes, feedback, score)
        
def attempt_pipeline(i):
    attempt_scratch_path = os.path.join(args.scratch_path, f"attempt{i}")
    with records_directory(attempt_scratch_path):
        # Actually run the pipeline and generate the patch (as a string)
        
        codebase_contents_count = count_files_and_dirs(args.codebase_path)
        record("pipeline_inputs/codebase_contents", codebase_contents_count, True)
        record("pipeline_inputs/llms", llms, True)
    
        try:
            pipeline_module = importlib.import_module(f"pipelines.{args.pipeline}")
            patch_result = try_models(models, pipeline_module.patch_vulns,
                                      vulns,
                                      args.codebase_path,
                                      sanitizer_stderr=sanitizer_stderr,
                                      delta=delta,
                                      pov_blob=(pov_blob if args.pass_blob else None),
                                      #pov_harness if args.pass_harness else None,
                                      patch_validator=validate_patch,
                                      max_attempts=n_attempts,
                                      # pass this in, because we want to resort to other models if tests fail
                                      require_pass=args.require_tests_pass)
            if patch_result is None:
                patch = None
                passed = None
                score = None
            else:
                patch, passed, score = patch_result
            
            if args.require_tests_pass:
                assert passed, "After retries were exhausted, no patch was found that passed."
        except ImportError:
            raise ValueError(f"Module {args.pipeline} not found. We expect there to be a module named the same thing as the pipeline.")
        except AttributeError:
            raise ValueError(f"Module {args.pipeline} does not have a 'generate_patch' function. We expect it to.")
        except Exception as e:
            raise e

        record(f"generated_patch", patch, False)

        #for llm_name, model in zip(llms, models):
        #    history = model.history
        #    record(f"{llm_name}_history", str(history), False)

        return (patch, passed, score)

def assert_good_patch(patch_tuple, attempt):
    patch, passed, score = patch_tuple
    assert passed, "The patch did not pass validation."
    
# We'll keep the outer loop of attempts for now, but it's not clear it'll be useful
final_patch, final_passed, final_score = attempt_n_times(attempt_pipeline, OUTER_RETRIES + 1, assert_good_patch, "Patch generation")

write_to_file(args.patch_path, final_patch)

# For sanity checking
final_patch_contents = read_file(args.patch_path)
print()
print("Contents of final patch:")
print(final_patch_contents)
print()

if final_score > 0:
    print("Final patch passed APPLY.")
if final_score > 1:
    print("Final patch passed BUILD.")
if final_score > 2:
    print("Final patch passed POV.")
if final_score > 3:
    print("Final patch passed FUNCTIONALITY.")
if final_passed:
    print("Final patch passed all available tests.")

print(f"Wrote final patch to {args.patch_path}")
