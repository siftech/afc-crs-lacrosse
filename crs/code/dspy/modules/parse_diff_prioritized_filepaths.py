import dspy
import models as models
from modules.mixins import BuggyCommitDeltaInput, HintsInput
# from caching import cache_predictions
from typing import List, Optional, Dict, Tuple

class ParseDiffPrioritizedFilepathsSignature(dspy.Signature, BuggyCommitDeltaInput, HintsInput):
    """Parse the output of the buggy git commit delta (a diff) to extract a list of all filepaths that were altered by the commit, and prioritize the list of filepaths by how likely the file is to need to be repaired to fix any cyber security vulnerabilities that this commit introduced or exposed. Filepaths more likely to need repair should be listed higher in the prioritized list, but the list should include ALL filepaths associated with hunks in the diff."""
    prioritized_filepaths: List[str] = dspy.OutputField(desc="A list of all codebase filepaths altered by the commit according to the diff, prioritized by how likely they are to require repair to fix cyber security vulnerabilities introduced or exposed by the commit (filepaths more likely to require fixing should show up higher in the list).")
    
# @cache_predictions(use_cache=True)
class ParseDiffPrioritizedFilepaths(dspy.Module):
    def __init__(self, verbose=False):
        self.verbose = verbose
        self.parser = dspy.Predict(ParseDiffPrioritizedFilepathsSignature)
        
    def forward(self, diff, hints="None."):
        pred = self.parser(buggy_commit_delta=diff, hints=hints)
        return dspy.Prediction(prioritized_filepaths=pred.prioritized_filepaths)

def parse_diff_prioritized_filepaths(diff, hints="None."):
    parser = ParseDiffPrioritizedFilepaths()
    pred = parser(diff=diff, hints=hints)
    return pred.prioritized_filepaths
