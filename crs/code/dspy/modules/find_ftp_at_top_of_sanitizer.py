import dspy
import models as models
from utils import place_source_path, find_relative_path_embedded
from modules.parse_sanitizer_prioritized_filepaths import parse_sanitizer_prioritized_filepaths
from modules.parse_diff_prioritized_filepaths import parse_diff_prioritized_filepaths
# from caching import cache_predictions
from typing import List, Optional, Dict, Tuple
from code_scan import filter_by_language, detect_language

#class FindFtpAtTopOfSanitizerSignature(dspy.Signature, SanitizerInput):

class FindFtpAtTopOfSanitizer(dspy.Module):
    def __init__(self, verbose=False):
        self.verbose = verbose
        
    def forward(self, sanitizer_stderr, codebase_path, delta=None, pov_harness_name=None):
        ftp_filepaths = []

        language = detect_language(codebase_path)
        print("detected language for codebase:", language)
        
        # Get the file-function pairs, in order, from the sanitizer
        sani_filepaths = parse_sanitizer_prioritized_filepaths(sanitizer_stderr, pov_harness_name=pov_harness_name)
        print("raw (parsed) sanitizer filepaths:")
        for filepath in sani_filepaths:
            print(filepath)
        print()
        print("codebase_path=", codebase_path)

        print()
        sani_filepaths = filter_by_language(sani_filepaths, language)
        print("sani_filepaths filtered by language is:")
        for fp in sani_filepaths:
            print(fp)
        print()
        
        print("sanitizer filepaths is:")
        for filepath in sani_filepaths:
            actual_filepath, rel_filepath = place_source_path(filepath, codebase_path)
            print("place_source_path returned relpath", rel_filepath)
            if rel_filepath is None:
                actual_filepath, rel_filepath = find_relative_path_embedded(filepath, codebase_path)
                print("find_relative_path_embedded returned relpath", rel_filepath)
            if rel_filepath is not None:
                print(rel_filepath)
                ftp_filepaths.append(rel_filepath)
            else:
                print(f"{rel_filepath} (removed)")

        # if we have a delta, parse it to get the file-function pairs then use that list to filter the first list
        if delta is not None:
            delta_filepaths = parse_diff_prioritized_filepaths(delta)
            print()
            print("delta filepaths is:")
            non_null_delta_filepaths = []
            for filepath in delta_filepaths:
                if filepath is not None:
                    print(filepath)
                    non_null_delta_filepaths.append(filepath)
                else:
                    print(f"{filepath} (removed)")

            print()
            non_null_delta_filepaths = filter_by_language(non_null_delta_filepaths, language)
            print("non_null_delta_filepaths filtered by language is:")
            for fp in non_null_delta_filepaths:
                print(fp)
            print()
                    
            # check if we have any filepaths from the sani
            if ftp_filepaths:
                ftp_filepaths_int = [ftp_filepath for ftp_filepath in ftp_filepaths if ftp_filepath in non_null_delta_filepaths]
                # only set it to the intersection if they intersect
                if ftp_filepaths_int:
                    ftp_filepaths = ftp_filepaths_int
                elif non_null_delta_filepaths:
                    # if there is no intersection, default to the delta
                    ftp_filepaths = non_null_delta_filepaths
                else:
                    # if there are no non_null_delta_filepaths, default to sani
                    pass
            # if there are no sani filepaths, go with the delta filepaths
            else:
                ftp_filepaths = non_null_delta_filepaths

        print()
        print("ftp_filepaths():")
        for ftp_filepath in ftp_filepaths:
            print(ftp_filepath)
        print()

        return dspy.Prediction(ftp_filepath=ftp_filepaths[0])

# Try it with more and more competent models until one can get the format right
def find_ftp_at_top_of_sanitizer(sanitizer_stderr, codebase_path, delta=None, pov_harness_name=None):
    finder = FindFtpAtTopOfSanitizer()
    pred = finder(sanitizer_stderr=sanitizer_stderr, codebase_path=codebase_path, delta=delta, pov_harness_name=pov_harness_name)
    return pred.ftp_filepath
