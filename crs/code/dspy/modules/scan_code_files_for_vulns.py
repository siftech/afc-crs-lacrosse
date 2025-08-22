import os
import json
import dspy
import models as models
from utils import read_file, well_formed_delta, place_source_path, current_dspy_lm
from modules.find_vuln_in_ftp import find_vuln_in_ftp
from modules.mixins import CodeInput, CodeFilepathInput, SanitizerListOptionalInput, PrioritizedVulnerabilitiesOutput
from modules.deduplicate_vulns import deduplicate_vulns
from modules.vuln_filter import vuln_filter
from modules.score_vuln import score_vuln
from code_scan import find_source_files, C, JAVA, detect_language
from record import record, get_records_directory

class ScanForVulnsSignature(dspy.Signature, CodeInput, CodeFilepathInput, SanitizerListOptionalInput, PrioritizedVulnerabilitiesOutput):
    """Acting as an expert cybersecurity researcher, find any cybersecurity vulnerabilities in the code file whose contents and relative path are provided. Prioritize the list of vulnerabilities by their confidence, severity, clarity, and likelihood to be exploited (most confident/severe/clear/likely should come first). Only include likely vulnerabilities that have a high probability of being exploitable — particularly those involving memory issues, unvalidated or unsafe input, arbitrary code execution, or potential backdoor functionality. Include issues that could be caught by one of the provided sanitizers, if applicable. Do not include speculative vulnerabilities or issues that do not exist directly in this code. There may not be any such vulnerabilities present, in which case the list should be empty."""

class ScanCodeFilesForVulns(dspy.Module):
    def __init__(self, verbose=False):
        self.verbose = verbose
        self.scanner = dspy.Predict(ScanForVulnsSignature)

    def name(self):
        model_name = current_dspy_lm()
        return f"scan_code_files_for_vulns_{model_name}"
        
    def record(self, record_name, content):
        record(f"{self.name()}/{record_name}", content, True)
        
    def forward(self, codebase_path, sanitizer_names=None, known_vulns_json=None, hints="None."):
        print("ScanCodeFilesForVulns (forward method)")

        language = detect_language(codebase_path)
        print("detected language for codebase:", language)
        
        vulns = []
        total_filtered = 0
        for i, (filepath, lang) in enumerate(find_source_files(codebase_path, language)):
            print(f"considering file {i}: {filepath}")
            try:
                with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
                    code = f.read()
                    abs_path, rel_path = place_source_path(filepath, codebase_path)
                    assert os.path.exists(abs_path), f"The scan file path doesn't point to an actual file: {abs_path}"
                    print("scan file relpath=", rel_path)
                    print("scan file abspath=", abs_path)
                    pred = self.scanner(code=code, code_filepath=rel_path, sanitizer_list=sanitizer_names)
                    file_vulns = pred.prioritized_vulnerabilities
                    print(f"{len(file_vulns)} file vulns found using original prompt.")
                    self.record(f"file_vulns_{rel_path}", "\n\n###############\n\n".join([str(vuln) for vuln in file_vulns]))
                    
                    # add incremental dedupe here
                    if known_vulns_json is not None:
                        file_vulns, _ = deduplicate_vulns(file_vulns, known_vulns_json, None)
                        print(f"{len(file_vulns)} file vulns found after deduplication.")
                        self.record(f"deduped_file_vulns_{rel_path}", "\n\n###############\n\n".join([str(vuln) for vuln in file_vulns]))

                    # Filter the new vulns by their details
                    n_before_filter = len(file_vulns)
                    file_vulns = [file_vuln for file_vuln in file_vulns if vuln_filter(file_vuln, sanitizer_names)]
                    n_after_filter = len(file_vulns)
                    n_filtered = n_before_filter - n_after_filter
                    print(f"{n_filtered} file vulns filtered out.")
                    total_filtered += n_filtered
                    print(f"{total_filtered} total vulns filtered out so far...")
                    
                    vulns = vulns + file_vulns
                    print(f"{len(vulns)} total vulns found so far...")

                    # Score the vulns
                    file_vulns = [score_vuln(file_vuln, sanitizer_names) for file_vuln in file_vulns]
                    
                    # LAST-MINUTE HACK! Ain't got time to bleed.
                    # Hijack the records_directory to store incremental vulns...
                    rec_dir = get_records_directory()
                    os.makedirs(rec_dir, exist_ok=True) # just to be safe
                    new_vulns_filename = f"vulns_in_file_{i}.json"
                    inc_path = os.path.join(rec_dir, new_vulns_filename)
                    if file_vulns:
                        try:
                            with open(inc_path, 'w') as f:
                                json.dump([file_vuln.model_dump() for file_vuln in file_vulns], f)
                            print("Incremental vulns stored at:", inc_path)
                        except Exception as e:
                            print(f"Error recording incremental vulns in file {inc_path}: {e}")
            except Exception as e:
                print(f"Caught error while scanning file {filepath}:")
                print(e)
                print("Ignore that file and keep scanning...")
        print(f"scan_code_files_for_vulns found {len(vulns)} vulns in total.")
        print(f"scan_code_files_for_vulns filtered out {total_filtered} vulns in total.")
        return dspy.Prediction(vulns=vulns)

def scan_code_files_for_vulns(codebase_path, sanitizer_names=None, known_vulns_json=None, hints="None."):
    print("scan_code_files_for_vulns")
    scanner = ScanCodeFilesForVulns()
    pred = scanner(codebase_path=codebase_path, sanitizer_names=sanitizer_names, known_vulns_json=known_vulns_json, hints=hints)
    return pred.vulns
