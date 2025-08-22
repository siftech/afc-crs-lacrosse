import os
from modules.discover_vulns import discover_vulns
from modules.patch_vulns import patch_vulns_w_feedback
#from modules.find_ftp_at_top_of_sanitizer import find_ftp_at_top_of_sanitizer
#from modules.rewrite_func_given_sanitizer_delta import rewrite_func_given_sanitizer_delta
#from modules.rewrite_func_given_sanitizer_blob_delta import rewrite_func_given_sanitizer_blob_delta
#from modules.rewrite_func_given_sanitizer_delta_blob_harness import rewrite_func_given_sanitizer_delta_blob_harness
from diff import generate_repo_diff
from record import record_before_and_after, get_records_directory, record

# Note: All pipelines will assume a non-null codebase_path, but this one additionally
# assumes a non-null sanitizer_stderr, at least for now.  Assuming it stays this way,
# we will want to dispatch to this only in cases where we have a sanitizer_stderr.

DEFAULT_MAX_ATTEMPTS = 5

def generate_patch(codebase_path, sanitizer_stderr=None, delta=None, pov_blob=None, pov_harness=None, discover_hints="None.", implement_hints="None.", patch_validator=None, max_attempts=DEFAULT_MAX_ATTEMPTS):
    print("discover_then_patch_vulns_w_feedback: generate_patch", type(codebase_path), type(sanitizer_stderr), type(delta), type(pov_blob), type(pov_harness), patch_validator)

    record(f"pipeline_inputs/codebase_path", codebase_path, True)
    record(f"pipeline_inputs/sanitizer_stderr", sanitizer_stderr, True)
    record(f"pipeline_inputs/delta", delta, True)
    record(f"pipeline_inputs/pov_blob", pov_blob, True)
    record(f"pipeline_inputs/pov_harness", pov_harness, True)
    record(f"pipeline_inputs/discover_hints", discover_hints, True)
    record(f"pipeline_inputs/implement_hints", implement_hints, True)
    record(f"pipeline_inputs/patch_validator", patch_validator, True)

    # Step 1: Discover Vulnerabilities
    vulns = discover_vulns(codebase_path, sanitizer_stderr=sanitizer_stderr, delta=delta, pov_blob=pov_blob, pov_harness=pov_harness, hints=discover_hints)
    print("vulns discovered:")
    for vuln in vulns:
        print(vuln)

    # FIXME: For now, just take the top one and discard the rest.
    #top_vuln = vulns[0]
    #file_repairs = top_vuln.suggested_file_repairs
    #if len(file_repairs) != 1:
    #    print(f"WARNING: For now we expect to find exactly one file to repair in the discovered vulnerability, but we found {len(file_repairs)}")

    #Step 2: Patch Vulnerabilities
    # For now, we patch all the found vulnerabilities in one big patch.  Not clear this is what we want, but a decent catch-all heuristic for now. Alternatively, we could call patch_vuln on just one.
    patch, validated, score = patch_vulns_w_feedback(vulns, codebase_path, sanitizer_stderr=sanitizer_stderr, pov_blob=pov_blob, delta=delta, hints=implement_hints, patch_validator=patch_validator, max_attempts=max_attempts)

    return {
        "patch": patch,
        "validated": validated,
        "score": score
    }
