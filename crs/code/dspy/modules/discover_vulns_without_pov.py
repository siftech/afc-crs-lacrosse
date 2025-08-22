import dspy
import models as models
from utils import well_formed_delta, well_formed_sanitizer_stderr
from modules.extract_delta_functions import extract_delta_functions
from modules.scan_code_files_for_vulns import scan_code_files_for_vulns
from modules.mixins import BuggyCommitDeltaInput, DeltaFunctionBodyMapInput, SanitizerListOptionalInput, PrioritizedVulnerabilitiesOutput, TargetLanguageInput
from modules.vuln_filter import vuln_filter
from modules.score_vuln import score_vuln
from code_scan import filter_by_language, detect_language

class HypothesizeVulnsFromDeltaFnsSignature(dspy.Signature, BuggyCommitDeltaInput, DeltaFunctionBodyMapInput, TargetLanguageInput, SanitizerListOptionalInput, PrioritizedVulnerabilitiesOutput):
        """Acting as an expert cyber security researcher, find any cyber security vulnerabilities introduced or exposed by the recent git commit whose unified diff (a.k.a. delta) is provided. For reference, also provided are the bodies of all modified functions for files modified by the diff, in the form of a map from file to function bodies. Prioritize the list of vulnerabilities by their confidence, severity, clarity, and likelihood to be exploited (most confident/severe/clear/likely should come first). Only include likely vulnerabilities that have a high probability of being exploitable — particularly those involving memory issues, unvalidated or unsafe input, arbitrary code execution, or potential backdoor functionality. Include issues that could be caught by one of the provided sanitizers, if applicable. Do not include speculative vulnerabilities or issues that do not exist directly in this code. Only include vulnerabilities that exist in code written in the target language."""

class DiscoverVulnsWithoutPov(dspy.Module):
    def __init__(self, verbose=False):
        self.verbose = verbose
        self.discoverer = dspy.Predict(HypothesizeVulnsFromDeltaFnsSignature)
        
    def forward(self, codebase_path, delta=None, sanitizer_names=None, known_vulns_json=None, hints="None."):
        # Get the functions/files that show up in the diff
        # Extract all the functions

        language = detect_language(codebase_path)
        print("detected language for codebase:", language)
            
        if well_formed_delta(delta):
            delta_fn_defs = extract_delta_functions(codebase_path, delta)
            print("proceed to discover")
            pred = self.discoverer(buggy_commit_delta=delta, delta_function_body_map=delta_fn_defs, sanitizer_list=sanitizer_names, target_language=language)
            vulns = pred.prioritized_vulnerabilities
            print(f"{len(vulns)} vulns found.")

            print("filtering by language", language)
            right_language_vulns = []
            for vuln in vulns:
                files = [sfr.target_filepath for sfr in vuln.suggested_file_repairs]
                filtered_files = filter_by_language(files, language)
                if len(filtered_files) > 0:
                    right_language_vulns.append(vuln)
                else:
                    print(f"Vuln eliminated because it's not the correct language: {str(vuln)}")
            vulns = right_language_vulns
            print(f"{len(vulns)} vulns after language-based filtering")

            # Filter the new vulns by their details
            # Not completely convinced we should be doing this here in the delta case.
            # But it does help guard against the zero-vuln trick-question CT...
            n_before_filter = len(vulns)
            vulns = [vuln for vuln in vulns if vuln_filter(vuln, sanitizer_names)]
            n_after_filter = len(vulns)
            n_filtered = n_before_filter - n_after_filter
            print(f"{n_filtered} vulns filtered out.")

            # Score the vulns
            vulns = [score_vuln(vuln, sanitizer_names) for vuln in vulns]
            
            print(f"returning {len(vulns)} vulns.")
        else:
            vulns = scan_code_files_for_vulns(codebase_path, sanitizer_names=sanitizer_names, known_vulns_json=known_vulns_json, hints=hints)
        return dspy.Prediction(vulns=vulns)

def discover_vulns_without_pov(codebase_path, delta=None, sanitizer_names=None, known_vulns_json=None, hints="None."):
    print("discover_vulns_without_pov")
    discoverer = DiscoverVulnsWithoutPov()
    pred = discoverer(codebase_path=codebase_path, delta=delta, sanitizer_names=sanitizer_names, known_vulns_json=known_vulns_json, hints=hints)
    return pred.vulns
