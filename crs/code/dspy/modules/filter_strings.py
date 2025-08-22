import dspy
import models as models
# from caching import cache_predictions
from typing import List

class FilterStringsSignature(dspy.Signature):
    """Filter the list according to the criterion."""
    strings: List[str] = dspy.InputField(desc="A list of input strings.")
    criterion: str = dspy.InputField(desc="A condition to filter on.")
    hint: str = dspy.InputField(desc="Tips for filtering.")
    filtered: List[str] = dspy.OutputField(desc="The subset of the input strings which meet the criterion. Make sure they match the corresponding input strings verbatim.")

class FilterStrings(dspy.Module):
    def __init__(self, verbose=False):
        self.verbose = verbose
        self.filterer = dspy.Predict(FilterStringsSignature)
        
    def forward(self, strings, criterion, hint="None."):
        pred = self.filterer(strings=strings, criterion=criterion, hint=hint)
        return dspy.Prediction(filtered=pred.filtered)

filterer = FilterStrings()

@models.with_escalation([models.gpt4o_10000, models.sonnet37, models.gpt41, models.sonnet4, models.gemini25pro])
def filter_strings(strings, criterion, hint="None."):
    pred = filterer(strings=strings, criterion=criterion, hint=hint)
    return pred.filtered
