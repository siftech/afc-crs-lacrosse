import os
from modules.discover_vulns import discover_vulns
from modules.patch_vulns import patch_vulns
from diff import generate_repo_diff
from record import record_before_and_after, get_records_directory, record

# Note: All pipelines will assume a non-null codebase_path, but this one additionally
# assumes a non-null sanitizer_stderr, at least for now.  Assuming it stays this way,
# we will want to dispatch to this only in cases where we have a sanitizer_stderr.

def generate_vulns(codebase_path, sanitizer_stderr=None, delta=None, pov_blob=None, pov_harness=None, pov_harness_name=None, sanitizer_names=None, known_vulns_json=None, known_inferior_vulns_json=None, hints="None."):
    print("characterize_vulns.generate_vulns", type(codebase_path), type(sanitizer_stderr), type(delta), type(pov_blob), type(pov_harness))

    record(f"pipeline_inputs/codebase_path", codebase_path, True)
    record(f"pipeline_inputs/sanitizer_stderr", sanitizer_stderr, True)
    record(f"pipeline_inputs/delta", delta, True)
    record(f"pipeline_inputs/pov_blob", pov_blob, True)
    record(f"pipeline_inputs/pov_harness", pov_harness, True)
    record(f"pipeline_inputs/pov_harness_name", pov_harness_name, True)
    record(f"pipeline_inputs/sanitizer_names", sanitizer_names, True)
    record(f"pipeline_inputs/known_vulns_json", known_vulns_json, True)
    record(f"pipeline_inputs/known_inferior_vulns_json", known_inferior_vulns_json, True)
    record(f"pipeline_inputs/hints", hints, True)

    # Pass through to the module for discover_vulns
    vulns, inferior_dupe_desc_map = discover_vulns(codebase_path, sanitizer_stderr=sanitizer_stderr, delta=delta, pov_blob=pov_blob, pov_harness=pov_harness, pov_harness_name=pov_harness_name, sanitizer_names=sanitizer_names, known_vulns_json=known_vulns_json, known_inferior_vulns_json=known_inferior_vulns_json, hints=hints)
    print("vulns discovered:")
    for vuln in vulns:
        print(vuln)

    print()
    print("inferior_dupe_desc_map:")
    if inferior_dupe_desc_map:
        for entry in inferior_dupe_desc_map:
            vuln_desc = entry[0]
            dupe_desc = entry[1]
            print()
            print(vuln_desc)
            print("--inferior_dupe--")
            print(dupe_desc)

    return vulns, inferior_dupe_desc_map
