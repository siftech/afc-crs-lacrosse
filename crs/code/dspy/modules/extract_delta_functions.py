import os
import json
import dspy
import models as models
from record import record
from utils import read_file, place_source_path, current_dspy_lm
from modules.extract_functions import extract_functions
#from modules.parse_diff_prioritized_filepaths import parse_diff_prioritized_filepaths
from modules.find_delta_files_and_functions import find_delta_files_and_functions

class ExtractDeltaFunctions(dspy.Module):
    def __init__(self, verbose=False):
        self.verbose = verbose

    def name(self):
        model_name = current_dspy_lm()
        return f"extract_delta_functions_{model_name}"
        
    def record(self, record_name, content):
        record(f"{self.name()}/{record_name}", content, False)    
        
    def forward(self, codebase_path, delta, hints="None."):
        # Get the functions/files that show up in the diff
        #delta_filepaths = parse_diff_prioritized_filepaths(delta)
        #print("delta filepaths is:")
        #for filepath in delta_filepaths:
        #    print(filepath)

        self.record("delta", delta)
        delta_fn_map = find_delta_files_and_functions(delta)
        self.record("delta_fn_map", json.dumps(delta_fn_map, indent=2))

        fn_def_map = {}
        
        for file_relpath, fn_names in delta_fn_map.items():
            print("repair file relpath:", file_relpath)
            file_abspath = os.path.join(codebase_path, file_relpath)
            print("repair file abspath=", file_abspath)
            assert os.path.exists(file_abspath), f"The delta file absolute path doesn't point to an actual file: {file_abspath}"
            file_contents = read_file(file_abspath)
            
            fn_defs = extract_functions(file_contents, fn_names, prefix=self.name())
            fn_def_map[file_relpath] = fn_defs
        
        return dspy.Prediction(fn_map=fn_def_map)

def extract_delta_functions(codebase_path, delta, hints="None."):
    print("extract_delta_functions")
    extractor = ExtractDeltaFunctions()
    pred = extractor(codebase_path=codebase_path, delta=delta, hints=hints)
    return pred.fn_map
