import os
import json
from itertools import chain
from collections import defaultdict
import dspy
import models as models
from typing import List
from utils import read_codebase_file, well_formed_delta, well_formed_blob, well_formed_sanitizer_stderr, replace_substrings, current_dspy_lm
from modules.extract_functions import extract_functions
from modules.map_strs import map_strs_gpt4o
from modules.filter_strs_by_index import filter_strs_by_index_gpt4o
from custom_types import Vulnerability
from modules.mixins import VulnerabilitiesInput, FunctionDefinitionsInput, BadPatchesWithFeedbackInput, SanitizerStderrOptionalInput, BlobOptionalInput, BuggyCommitDeltaOptionalInput, HintsInput
from record import record

# Base class
class ReplanFileRepairsForVulnsSignature(dspy.Signature, VulnerabilitiesInput, FunctionDefinitionsInput, BadPatchesWithFeedbackInput, SanitizerStderrOptionalInput, BlobOptionalInput, BuggyCommitDeltaOptionalInput, HintsInput):
    """Acting as an expert cyber security researcher, replan the file repairs for one or more of the hypothesized vulnerabilities, taking into account the previous patching attempt(s) and the feedback it/they received. You may receive additional details about the true vulnerability that the vulnerability hypotheses are trying to address."""
    revised_vulnerabilities: List[Vulnerability] = dspy.OutputField(desc="A revised set of vulnerability hypotheses, modified to result in a better patch for the true vulnerability. The modifications may include modifying the repair plan(s) for some existing vulnerability hypotheses, and/or filtering out some vulnerability hypotheses entirely. However, this revised list should not be empty, so do not filter out all vulnerability hypotheses without adding one or more new ones.")

class ReplanFileRepairsForVulns(dspy.Module):
    def __init__(self, verbose=False):
        self.verbose = verbose
        self.replanner = dspy.Predict(ReplanFileRepairsForVulnsSignature)

    def name(self):
        model_name = current_dspy_lm()
        return f"replan_file_repairs_for_vulns_{model_name}"

    def record(self, record_name, content):
        record(f"{self.name()}/{record_name}", content, True)

    def forward(self, vulns, bad_patches_w_feedback, codebase_path, sanitizer_stderr=None, pov_blob=None, delta=None, hints="None."):
        print("replan_file_repairs_for_vulns forward method")
        self.record("input_vulns", "\n=================\n".join([str(vuln) for vuln in vulns]))
        self.record("bad_patches_w_feedback", "\n=================\n".join([f"{obj.feedback}\n\n{obj.patch}" for obj in bad_patches_w_feedback]))

        all_file_fns = [(repair.target_filepath, fn_name) for vuln in vulns for repair in vuln.suggested_file_repairs for fn_name in repair.target_function_names]
        unique_file_fns = list(set(all_file_fns))
        file_fn_map = defaultdict(list)
        for filepath, fn in unique_file_fns:
            file_fn_map[filepath].append(fn)

        self.record("file_fn_map", json.dumps(file_fn_map, indent=2))

        fn_defs = []
        
        for filepath, file_fns in file_fn_map.items():
            filecode = read_codebase_file(filepath, codebase_path)
            file_fn_defs = extract_functions(filecode, file_fns, prefix=self.name())
            fn_defs = fn_defs + file_fn_defs
        
        self.record("original_functions", "\n=================\n".join(fn_defs))
        print("fn_defs length:", len(fn_defs))
        
        repred = self.replanner(vulnerabilities=vulns, bad_patches_with_feedback=bad_patches_w_feedback, function_definitions=fn_defs, sanitizer_stderr=sanitizer_stderr, blob=pov_blob, buggy_commit_delta=delta, hints=hints)

        rev_vulns = repred.revised_vulnerabilities
        self.record("revised_vulns", "\n=================\n".join([str(vuln) for vuln in rev_vulns]))

        return dspy.Prediction(revised_vulnerabilities=rev_vulns)

def replan_file_repairs_for_vulns(vulns, bad_patches_w_feedback, codebase_path, sanitizer_stderr=None, pov_blob=None, delta=None, hints="None."):
    print("replan_file_repairs_for_vulns")
    replanner = ReplanFileRepairsForVulns()
    pred = replanner(vulns=vulns, bad_patches_w_feedback=bad_patches_w_feedback, codebase_path=codebase_path, sanitizer_stderr=sanitizer_stderr, pov_blob=pov_blob, delta=delta, hints=hints)
    return pred.revised_vulnerabilities
