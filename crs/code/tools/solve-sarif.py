#!/usr/bin/env python3

# June 16th revision...
# Solve sarif sans pov. If we don't get a pov, we hand over the sarif and codebase path to the LLM.
# We ask the LLM if the sarif is correct and submit the broadcast sarif.
# If we are given an pov we compare the functions of the pov to the sarif to determine
# if the sarif describes a true pov. We ask the LLM. We submit the broadcast sarif,
# and we can potentially submit a bundle as well.
# hyps_json_path becomes pov.

# Pulling similar logic from tools/patch-vulns.py

# FIXME Needed?
DEFAULT_MAX_RETRIES = 3

# Note: Outer retries start from scratch and do not run tests, unlile the inner-loop
# retries (above) which are the primary mechanism.
OUTER_RETRIES = 0

# FIXME Do we want to telemetry here?
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
from parse_reproduce import parse_sanitizer_stderr_with_instrumentation, clean_fuzzing_output

# Import solve_sarif module
from modules.solve_sarif import assess_sarif
from modules.solve_sarif_sans_pov import assess_sarif_sans_pov

# Set up the argument parser
# For Sarifs: Vulnerability class object, sarif json and assessment output path
# Optional Args: list of llms and scratch. FIXME Missing optional args?
parser = argparse.ArgumentParser(description="Process three required arguments:codebase path, sarif json object, assessment output path" \
    "Optional keyword args: pov,llms, scratch")

# Required positional arguments
# parser.add_argument("hyps_json_path", help="The path to the json file storing vulnerabilties to be assessed.")
parser.add_argument("codebase_path", help="The path to the codebase to be patched.")
parser.add_argument("sarif_json_path", help="The filepath of the json object of the broadcast sarif to be assessed.")
parser.add_argument("sarif_assessment_path", help="The path to the assessment to be produced.")
parser.add_argument("-pov", "--pov", help="The path to the json file storing vulnerabilties to be assessed.")
parser.add_argument("-llms", "--llms", nargs="+", help="A list of llm names to use, in the order we want to resort to them in (on failure)", default="sonnet37 gpt4o")
parser.add_argument("-scratch", "--scratch_path", help="Path to dir for storing intermediate outputs and logs of patch generation", default="assess-finals-sarif-scratch")

# Parse arguments
args = parser.parse_args()

# Print parsed values
print("solve-sarif.py received args:")
# print(f"hyps_json_path: {args.hyps_json_path}")
print(f"codebase_path: {args.codebase_path}")
print(f"sarif_json_path: {args.sarif_json_path}")
print(f"sarif_assessment_path: {args.sarif_assessment_path}")
print(f"pov: {args.pov}")
print(f"llms: {args.llms}")
print(f"scratch_path (-scratch): {args.scratch_path}")

# LLMs
llms = args.llms
models = [find_model(llm) for llm in llms]

with open(args.sarif_json_path) as f:
    sarif_data = json.load(f)
    print("solve-sarif.py DEBUG - sarif_data dict is:")
    print(json.dumps(sarif_data, indent=2))

# Look at the Vulnerability object in the list of length one and save it as our "vuln"
if args.pov:
    with open(args.hyps_json_path) as f:
        vulns_data = json.load(f)
        for i, vuln_dict in enumerate(vulns_data):
            print(f"vuln_dict({i})=")
            print(json.dumps(vuln_dict, indent=2))
            vuln = Vulnerability.model_validate(vuln_dict)
            print(f"solve-sarif.py DEBUG -  vuln is: {vuln}")

def attempt_sarif_assessment(i):
    attempt_scratch_path = os.path.join(args.scratch_path, f"attempt{i}")
    with records_directory(attempt_scratch_path):
        # FIXME run the pipeline? want to return with a 'correct' or 'incorrect' assessment
        # Currently, we don't hold the a pipeline for pipeline_module.solve_sarif like patch-vulns (patch_vulns) does.
        # We can introduce a pipeline if deemed needed...
        # LLM pipeline
        record("pipeline_inputs/llms", llms, True)

        # Try each model runnning the dspy module 'assess_sarif' func from dspy/modules/solve_sarif
        # This takes holds the initial signature /prompt for asking the llm with the supporting vulns and sarif json.
        # We expect to get a "yes or no" response with reason : Dict[bool, str].
        # ... or call the extract_sanitizer_stderr_with_instrumentation ?
        if args.pov is not None:
            try:
                sarif_result = try_models(models, assess_sarif,
                                        vuln,
                                        args.codebase_path,
                                        sarif_data,
                                        # sanitizer_stderr=sanitizer_stderr,
                                        # delta=delta,
                                        # pov_blob=(pov_blob if args.pass_blob else None),
                )
            except Exception as e:
                raise e
        else:
            try:
                answer, reason = try_models(models, assess_sarif_sans_pov,
                                        args.codebase_path,
                                        sarif_data,
                                        # sanitizer_stderr=sanitizer_stderr,
                                        # delta=delta,
                                        # pov_blob=(pov_blob if args.pass_blob else None),
                )
                sarif_result = {
                    "decision": answer,
                    "description": reason
                }
            except Exception as e:
                raise e

        record("sarif result", json.dumps(sarif_result, indent=2), True)

        #for llm_name, model in zip(llms, models):
        #    history = model.history
        #    record(f"{llm_name}_history", str(history), False)

        return sarif_result


# FIXME change this based on what the lisp expects.
# Write the sarif assessment out to a path?
# Do this attempt_n_times()?
final_sarif_assessment = attempt_sarif_assessment(0)
write_to_file(args.sarif_assessment_path, json.dumps(final_sarif_assessment, indent=2))
print(f"Wrote sarif assessment to {args.sarif_assessment_path}")
