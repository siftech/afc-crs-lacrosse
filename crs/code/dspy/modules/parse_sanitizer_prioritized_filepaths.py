import dspy
import models as models
from modules.mixins import SanitizerStderrInput, HarnessNameOptionalInput, HintsInput
# from caching import cache_predictions
from typing import List, Optional, Dict, Tuple

class ParseSanitizerPrioritizedFilepathsSignature(dspy.Signature, SanitizerStderrInput, HarnessNameOptionalInput, HintsInput):
    """Parse the output of the address sanitizer to extract a list of all filepaths from the buggy codebase, prioritized by how likely the file is to contain the security vulnerability that needs to be repaired to prevent the sanitizer crash in the future. Do not list filepaths that are likely part of the fuzzer, the sanitizer, or the harness (if harness name is provided). Outside of these, do list the complete set of codebase filepaths that show up in the backtrace and/or the memory layout section (if applicable) and/or the instrumentation logging (if applicable). Filepaths more likely to need repair should be listed higher in the prioritized list. If applicable, filepaths may need to be inferred (e.g. from fully qualified class names if dealing with java)."""
    prioritized_filepaths: List[str] = dspy.OutputField(desc="A list of all mentioned codebase filepaths, prioritized by how likely they are to require repair to fix the vulnerability that caused the sanitizer crash (filepaths more likely to require fixing should show up higher in the list).")
    
# @cache_predictions(use_cache=True)
class ParseSanitizerPrioritizedFilepaths(dspy.Module):
    def __init__(self, verbose=False):
        self.verbose = verbose
        self.parser = dspy.Predict(ParseSanitizerPrioritizedFilepathsSignature)
        
    def forward(self, sanitizer_stderr, pov_harness_name=None, hints="None."):
        pred = self.parser(sanitizer_stderr=sanitizer_stderr, harness_name=pov_harness_name, hints=hints)
        return dspy.Prediction(prioritized_filepaths=pred.prioritized_filepaths)

def parse_sanitizer_prioritized_filepaths(sanitizer_stderr, pov_harness_name=None, hints="None."):
    parser = ParseSanitizerPrioritizedFilepaths()
    pred = parser(sanitizer_stderr=sanitizer_stderr, pov_harness_name=pov_harness_name, hints=hints)
    return pred.prioritized_filepaths

