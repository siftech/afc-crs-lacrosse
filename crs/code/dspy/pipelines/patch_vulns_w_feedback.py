from modules.patch_vulns import patch_vulns_w_feedback
from record import record

# Note: All pipelines will assume a non-null codebase_path, but this one additionally
# assumes a non-null sanitizer_stderr, at least for now.  Assuming it stays this way,
# we will want to dispatch to this only in cases where we have a sanitizer_stderr.

DEFAULT_MAX_ATTEMPTS = 5

def patch_vulns(vulns, codebase_path, sanitizer_stderr=None, delta=None, pov_blob=None, pov_harness=None, hints="None.", patch_validator=None, max_attempts=DEFAULT_MAX_ATTEMPTS, require_pass=False):
    print("patch_vulns_w_feedback: patch_vulns", type(vulns), type(codebase_path), type(sanitizer_stderr), type(delta), type(pov_blob), type(pov_harness), patch_validator)

    record(f"pipeline_inputs/vulns", [str(vuln) for vuln in vulns], True)
    record(f"pipeline_inputs/codebase_path", codebase_path, True)
    record(f"pipeline_inputs/sanitizer_stderr", sanitizer_stderr, True)
    record(f"pipeline_inputs/delta", delta, True)
    record(f"pipeline_inputs/pov_blob", pov_blob, True)
    record(f"pipeline_inputs/pov_harness", pov_harness, True)
    record(f"pipeline_inputs/hints", hints, True)
    record(f"pipeline_inputs/patch_validator", patch_validator, True)
    record(f"pipeline_inputs/max_attempts", max_attempts, True)
    record(f"pipeline_inputs/require_pass", require_pass, True)

    patch, validated, score = patch_vulns_w_feedback(vulns, codebase_path, sanitizer_stderr=sanitizer_stderr, pov_blob=pov_blob, delta=delta, hints=hints, patch_validator=patch_validator, max_attempts=max_attempts, require_pass=require_pass)

    return (patch, validated, score)
