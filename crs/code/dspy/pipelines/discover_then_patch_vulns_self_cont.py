import os
from modules.discover_vulns import discover_vulns
from modules.patch_vuln import patch_vuln
#from modules.find_ftp_at_top_of_sanitizer import find_ftp_at_top_of_sanitizer
#from modules.rewrite_func_given_sanitizer_delta import rewrite_func_given_sanitizer_delta
#from modules.rewrite_func_given_sanitizer_blob_delta import rewrite_func_given_sanitizer_blob_delta
#from modules.rewrite_func_given_sanitizer_delta_blob_harness import rewrite_func_given_sanitizer_delta_blob_harness
from diff import generate_repo_diff
from record import record_before_and_after, get_records_directory
import pipelines.discover_then_patch_vulns as discover_then_patch_vulns

# This is an example of a pipeline that just forwards along to another pipeline, but with a particular hint.

def generate_patch(codebase_path, sanitizer_stderr=None, delta=None, pov_blob=None, pov_harness=None):
    print("discover_then_patch: generate_patch", type(codebase_path), type(sanitizer_stderr), type(delta), type(pov_blob), type(pov_harness))

    self_contained_hint = "The set of repairs should result in code that can build without assuming outside modifications, e.g. it should not rely on undefined global variables."

    return discover_then_patch_vulns.generate_patch(codebase_path, sanitizer_stderr=sanitizer_stderr, delta=delta, pov_blob=pov_blob, pov_harness=pov_harness, implement_hints=self_contained_hint)
