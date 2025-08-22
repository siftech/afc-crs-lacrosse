import dspy
import models as models
from utils import well_formed_delta, well_formed_sanitizer_stderr, current_dspy_lm
from modules.discover_vulns_with_pov import discover_vulns_with_pov
from modules.discover_vulns_without_pov import discover_vulns_without_pov
from modules.deduplicate_vulns import deduplicate_vulns
from record import record

class DiscoverVulns(dspy.Module):
    def __init__(self, verbose=False):
        self.verbose = verbose

    def name(self):
        model_name = current_dspy_lm()
        return f"discover_vulns_{model_name}"
        
    def record(self, record_name, content, verbose=True):
        record(f"{self.name()}/{record_name}", content, verbose)    

    def forward(self, codebase_path, sanitizer_stderr=None, delta=None, pov_blob=None, pov_harness=None, pov_harness_name=None, sanitizer_names=None, known_vulns_json=None, known_inferior_vulns_json=None, hints="None."):
        if well_formed_sanitizer_stderr(sanitizer_stderr):
            # If we have a PoV we assume we're just finding a single vulnerability.
            # FIXME: We may want to move to a model where we are generating multipl hypotheses.
            vuln = discover_vulns_with_pov(codebase_path, sanitizer_stderr, pov_blob=pov_blob, pov_harness=pov_harness, pov_harness_name=pov_harness_name, delta=delta, hints=hints)
            vulns = [vuln]
        else:
            vulns = discover_vulns_without_pov(codebase_path, delta=delta, sanitizer_names=sanitizer_names, known_vulns_json=known_vulns_json, hints=hints)
        self.record("discovered_vulns", "\n\n###############\n\n".join([str(vuln) for vuln in vulns]))

        # We deduplicate here based on known vulns.
        # Unfortunately this means we could still generate dupes if they are being characterized simultaneously.
        # If there's time, could keep track and circle back to call deduplicate_vulns on those.
        inferior_dupe_desc_map = None
        if known_vulns_json is not None or known_inferior_vulns_json is not None:
            vulns, inferior_dupe_desc_map = deduplicate_vulns(vulns, known_vulns_json, known_inferior_vulns_json)
            self.record("deduped_vulns", "\n\n###############\n\n".join([str(vuln) for vuln in vulns]))

        return dspy.Prediction(vulns=vulns, inferior_dupe_desc_map=inferior_dupe_desc_map)

def discover_vulns(codebase_path, sanitizer_stderr=None, delta=None, pov_blob=None, pov_harness=None, pov_harness_name=None, sanitizer_names=None, known_vulns_json=None, known_inferior_vulns_json=None, hints="None."):
    print("discover_vulns")
    discoverer = DiscoverVulns()
    pred = discoverer(codebase_path=codebase_path, sanitizer_stderr=sanitizer_stderr, delta=delta, pov_blob=pov_blob, pov_harness=pov_harness, pov_harness_name=pov_harness_name, sanitizer_names=sanitizer_names, known_vulns_json=known_vulns_json, known_inferior_vulns_json=known_inferior_vulns_json, hints=hints)
    return pred.vulns, pred.inferior_dupe_desc_map
