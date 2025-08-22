#!/usr/bin/env python3

# A script entrypoint for the finals lax agent to hypothesize vulnerabilities via dspy

DEFAULT_MAX_RETRIES = 2

# Telemetry
import openlit
openlit.init(
    application_name="lacrosse",
)

import json
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
from parse_reproduce import clean_fuzzing_output
from utils import read_file, write_to_file, count_files_and_dirs, attempt_n_times
from parse_reproduce import parse_sanitizer_stderr_with_instrumentation

# Set up the argument parser
# For round 1 only the llms, delta, sani, and blob args will be supported
parser = argparse.ArgumentParser(description="Process two required arguments and five optional keyword arguments: pipeline, llms, delta, sani, blob, harn.")

# Required positional arguments
parser.add_argument("codebase_path", help="The path to the codebase to be patched.")
parser.add_argument("vulns_path", help="The path to the vulnerability hypotheses to be produced.")

# Optional keyword arguments
parser.add_argument("-pipeline", "--pipeline", help="The name of the pipeline to be used", default="characterize_vulns")
parser.add_argument("-scratch", "--scratch_path", help="Path to dir for storing intermediate outputs and logs of patch generation", default="gen-finals-patch-scratch")
parser.add_argument("-llms", "--llms", nargs="+", help="A list of llm names to use, in the order we want to resort to them in (on failure)", default="sonnet37 gpt4o")
parser.add_argument("-delta", "--delta_path", help="Path to delta (diff)", default=None)
parser.add_argument("-known", "--known_vulns_json", help="The path to the json file storing known vulnerabilties", default=None)
parser.add_argument("-knowninf", "--known_inferior_vulns_json", help="The path to the json file storing known inferior vulnerabilties", default=None)
parser.add_argument("-infpath", "--inferior_dupes_path", help="The inferior duplicates map (output).", default=None)
parser.add_argument("-sani", "--sanitizer_stderr_path", help="Path to sanitizer stderr log for pov", default=None)
parser.add_argument("-blob", "--blob_path", help="Path to pov blob, i.e. test case", default=None)
parser.add_argument("-harn", "--harness_name", help="Name of harness for pov", default=None)
parser.add_argument("-saniname", "--sanitizer_name", help="Name of the sanitizer, for testing the patch against the pov.", default=None)
parser.add_argument("-saninames", "--sanitizer_names", nargs="+", help="Names of the available sanitizers.", default=None)
parser.add_argument("-fuzzeng", "--fuzzing_engine", help="Name of the fuzzing engine, for testing the patch against the pov.", default=None)
parser.add_argument("-proj", "--project_name", help="The project name, for testing the patch against the pov.", default=None)
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
print(f"known_vulns_json (-known): {args.known_vulns_json}")
print(f"known_inferior_vulns_json (-knowninf): {args.known_inferior_vulns_json}")
print(f"inferior_dupes_path (-infpath): {args.inferior_dupes_path}")
print(f"sanitizer_stderr_path (-sani): {args.sanitizer_stderr_path}")
print(f"blob_path (-blob): {args.blob_path}")
print(f"harness_name (-harn): {args.harness_name}")
print(f"sanitizer_name (-saniname): {args.sanitizer_name}") # maybe OBE. prob inherited from patch-vulns.
print(f"sanitizer_names (-saninames): {args.sanitizer_names}") # for hyp task
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
        
def attempt_pipeline(i):
    attempt_scratch_path = os.path.join(args.scratch_path, f"attempt{i}")
    with records_directory(attempt_scratch_path):
        # Actually run the pipeline and generate the patch (as a string)

        codebase_contents_count = count_files_and_dirs(args.codebase_path)
        record("pipeline_inputs/codebase_contents", codebase_contents_count, True)
        record("pipeline_inputs/llms", llms, True)
    
        try:
            pipeline_module = importlib.import_module(f"pipelines.{args.pipeline}")
            vulns, inferior_dupe_desc_map = try_models(models, pipeline_module.generate_vulns,
                                                       args.codebase_path,
                                                       sanitizer_stderr,
                                                       delta,
                                                       pov_blob if args.pass_blob else None,
                                                       pov_harness if args.pass_harness else None,
                                                       args.harness_name,
                                                       args.sanitizer_names,
                                                       args.known_vulns_json,
                                                       args.known_inferior_vulns_json)
        except ImportError:
            raise ValueError(f"Module {args.pipeline} not found. We expect there to be a module named the same thing as the pipeline.")
        except AttributeError:
            raise ValueError(f"Module {args.pipeline} does not have a 'hypothesize_vulns' function. We expect it to.")
        except Exception as e:
            raise e

        dict_vulns = [vuln.model_dump() for vuln in vulns]
        record(f"hypothesized_vulns", json.dumps(dict_vulns, indent=2), True)

        record(f"inferior_dupe_desc_map", json.dumps(inferior_dupe_desc_map, indent=2), True)
        
        #for llm_name, model in zip(llms, models):
        #    history = model.history
        #    record(f"{llm_name}_history", str(history), False)

        return vulns, inferior_dupe_desc_map

    
def assert_good_vulns (vulns, attempt):
    assert vulns is not None, "vulns is None"
    # Getting rid of this assertion.  There may be a CT wih no vulns.  Also since we are now trying to avoid coming
    # up with duplicate vulns, it will be common to come up with zero, and retrying opens us up to false-negative duplicates.
    # assert len(vulns) > 0, "The vulns have length 0"

final_vulns, final_inferior_dupe_desc_map = attempt_n_times(attempt_pipeline, n_attempts, assert_good_vulns, "Hypothesizing vulnerabilities")

final_dict_vulns = [vuln.model_dump() for vuln in final_vulns]
write_to_file(args.vulns_path, json.dumps(final_dict_vulns, indent=2))

write_to_file(args.inferior_dupes_path, json.dumps(final_inferior_dupe_desc_map, indent=2))

# For sanity checking
with open(args.vulns_path, "r") as f:
    final_vulns_contents = json.load(f)

print()
print("Contents of final vulns:")
print(final_vulns_contents)
print()

with open(args.inferior_dupes_path, "r") as f:
    inferior_dupes_contents = json.load(f)

print()
print("Contents of final inferior dupes:")
print(inferior_dupes_contents)
print()

if args.inferior_dupes_path:
    print(f"Wrote final inferior dupes to {args.inferior_dupes_path}")

print(f"Wrote final vulns to {args.vulns_path}")

