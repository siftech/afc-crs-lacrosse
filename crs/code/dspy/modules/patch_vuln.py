import os
import json
import dspy
import models as models
from record import get_records_directory, record
from diff import generate_repo_diff_for_repairs
from utils import well_formed_delta, well_formed_sanitizer_stderr, current_dspy_lm
from modules.implement_file_repair import implement_file_repair

class PatchVuln(dspy.Module):
    def __init__(self, verbose=False):
        self.verbose = verbose

    def name(self):
        model_name = current_dspy_lm()
        return f"patch_vuln_{model_name}"
        
    def record(self, record_name, content):
        record(f"{self.name()}/{record_name}", content, False)
                
    def forward(self, vuln, codebase_path, sanitizer_stderr=None, pov_blob=None, delta=None, hints="None."):
        file_repairs = vuln.suggested_file_repairs
        vuln_desc = vuln.description
        repair_map = {}
        
        for file_repair in file_repairs:
            file_relpath = file_repair.target_filepath
            print("repair file relpath:", file_relpath)
            file_abspath = os.path.join(codebase_path, file_relpath)
            print("repair file abspath=", file_abspath)
            assert os.path.exists(file_abspath), f"The repair file absolute path doesn't point to an actual file: {file_abspath}"
            original_code = read_file(file_abspath)

            new_file_code = implement_file_repair(file_repair, original_code, vuln, codebase_path, sanitizer_stderr=sanitizer_stderr, pov_blob=pov_blob, delta=delta, hints=hints)
            repair_map[file_relpath] = new_file_code
        
        # Finally, generate a unified diff using the repaired FTP
        patch_diff = generate_repo_diff_for_repairs(codebase_path, repair_map, base_dir=get_records_directory())

        print("patch length", len(patch_diff))
    
        assert len(patch_diff) > 0, "The patch is empty"

        return dspy.Prediction(patch=patch_diff)

def patch_vuln(vuln, codebase_path, sanitizer_stderr=None, pov_blob=None, delta=None, hints="None."):
    print("patch_vuln")
    patcher = PatchVuln()
    pred = patcher(vuln=vuln, codebase_path=codebase_path, sanitizer_stderr=sanitizer_stderr, pov_blob=pov_blob, delta=delta, hints=hints)
    return pred.patch
