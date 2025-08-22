import os
import json
import dspy
import models as models
from record import get_records_directory, record, records_directory
from custom_types import PatchWithFeedback
from diff import generate_repo_diff_for_repairs
from utils import well_formed_delta, well_formed_sanitizer_stderr, current_dspy_lm, read_file
from modules.implement_file_repair import implement_file_repair
from modules.replan_file_repairs_for_vulns import replan_file_repairs_for_vulns

class PatchVulns(dspy.Module):
    def __init__(self, verbose=False):
        self.verbose = verbose
        
    def name(self):
        model_name = current_dspy_lm()
        return f"patch_vulns_{model_name}"
        
    def record(self, record_name, content):
        record(f"{self.name()}/{record_name}", content, False)
        
    def forward(self, vulns, codebase_path, sanitizer_stderr=None, pov_blob=None, delta=None, hints="None."):
        print("PatchVulns(forward method)")
        repair_map = {}

        for i, vuln in enumerate(vulns):
            self.record(f"vuln_{i}", str(vuln))
            file_repairs = vuln.suggested_file_repairs
            vuln_desc = vuln.description
            
            for file_repair in file_repairs:
                file_relpath = file_repair.target_filepath
                print("repair file relpath:", file_relpath)
                file_abspath = os.path.join(codebase_path, file_relpath)
                print("repair file abspath=", file_abspath)
                assert os.path.exists(file_abspath), f"The repair file absolute path doesn't point to an actual file: {file_abspath}"
                original_code = read_file(file_abspath)
                self.record("original_code", original_code)

                if file_relpath in repair_map.keys():
                    file_code = repair_map[file_relpath]
                else:
                    file_code = original_code

                new_file_code = implement_file_repair(file_repair, file_code, vuln, codebase_path, sanitizer_stderr=sanitizer_stderr, pov_blob=pov_blob, delta=delta, hints=hints)
                
                repair_map[file_relpath] = new_file_code

        self.record("repair_map", json.dumps(repair_map, indent=2))

        records_dir = get_records_directory()
        
        # Finally, generate a unified diff using the repaired FTP
        patch_diff = generate_repo_diff_for_repairs(codebase_path, repair_map, base_dir=records_dir)
        
        print("patch length", len(patch_diff))
    
        assert len(patch_diff) > 0, "The patch is empty"

        return dspy.Prediction(patch=patch_diff)

def patch_vulns(vulns, codebase_path, sanitizer_stderr=None, pov_blob=None, delta=None, hints="None."):
    print("patch_vulns")
    patcher = PatchVulns()
    pred = patcher(vulns=vulns, codebase_path=codebase_path, sanitizer_stderr=sanitizer_stderr, pov_blob=pov_blob, delta=delta, hints=hints)
    return pred.patch            

# This version passes in a validator which is used to test the patchh and iterate if it isn't validated, using past feedback to generate new patches.
# Returns two values: the patch, and a boolean saying whether it was validated

DEFAULT_MAX_ATTEMPTS = 5

def patch_vulns_w_feedback(vulns, codebase_path, sanitizer_stderr=None, pov_blob=None, delta=None, hints="None.", patch_validator=None, max_attempts=DEFAULT_MAX_ATTEMPTS, require_pass=False):
    print("patch_vulns_w_feedback")
    print("patch_validator=", patch_validator)
    records_dir = get_records_directory()
    patcher = PatchVulns()
    
    if patch_validator is None:
        pred = patcher(vulns=vulns, codebase_path=codebase_path, sanitizer_stderr=sanitizer_stderr, pov_blob=pov_blob, delta=delta, hints=hints)
        return (pred.patch, None, None)
    else:
        bad_patches_w_feedback = []
        updated_vulns = vulns
        for i in range(0, max_attempts):
            pred = patcher(vulns=updated_vulns, codebase_path=codebase_path, sanitizer_stderr=sanitizer_stderr, pov_blob=pov_blob, delta=delta, hints=hints)
            print(f"Validate patch, attempt {i}.")
            val_dir = os.path.join(records_dir, f"validation{i}")
            print(f"Records dir for this validation attempt: {val_dir}.")
            with records_directory(val_dir):
                val_result = patch_validator(pred.patch, val_dir)
                print(f"val_result={val_result}")
                passed, patch_feedback, score = val_result
                print(f"patch_validator outputs: passed={passed} patch_feedback={patch_feedback} score={score}")
                if passed:
                    return (pred.patch, passed, score)

                bad_patches_w_feedback.append(PatchWithFeedback(patch=pred.patch, feedback=patch_feedback, score=score, passed=passed))
                # replan the file repairs in the vulns
                updated_vulns = replan_file_repairs_for_vulns(vulns, bad_patches_w_feedback, codebase_path, sanitizer_stderr=sanitizer_stderr, pov_blob=pov_blob, delta=delta, hints=hints)

        best_patch = max(bad_patches_w_feedback, key=lambda x: x.score)

        if require_pass:
            assert best_patch.passed, f"The patch was required to pass, but didn't after {max_attempts} attempts."
            
        return (best_patch.patch, best_patch.passed, best_patch.score)
