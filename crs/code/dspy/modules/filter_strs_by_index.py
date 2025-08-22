import dspy
import models as models
# from caching import cache_predictions
from typing import List

class FilterStrsByIndexSignature(dspy.Signature):
    """Filter the list according to the criterion, by providing the numbers associated with the qualifying strings."""
    choices: str = dspy.InputField(desc="A numbered list of input strings.")
    criterion: str = dspy.InputField(desc="A condition to filter on.")
    hint: str = dspy.InputField(desc="Extra tips for filtering, if any.")
    filtered: List[int] = dspy.OutputField(desc="The numbers associated with the input strings which meet the criterion.")

class FilterStrsByIndex(dspy.Module):
    def __init__(self, verbose=False):
        self.verbose = verbose
        self.filterer = dspy.Predict(FilterStrsByIndexSignature)

    def format_choices_string(self, snippets):
        """Formats a list of strings into a numbered list as a single string."""
        return "\n".join(f"{index + 1}. {item}" for index, item in enumerate(snippets))
        
    def forward(self, strings, criterion, hint="None."):
        choices_string = self.format_choices_string(strings)
        filterpred = self.filterer(choices=choices_string, criterion=criterion, hint=hint)
        zero_indexed = [index - 1 for index in filterpred.filtered]
        zero_indexed.sort()
        filtered = [strings[idx] for idx in zero_indexed]
        return dspy.Prediction(filtered=filtered)

@models.with_escalation([models.gpt4o_10000, models.sonnet37, models.gpt41, models.sonnet4, models.gemini25pro])
def filter_strs_by_index_gpt4o(strings, criterion, hint="None."):
    filterer = FilterStrsByIndex()
    pred = filterer(strings=strings, criterion=criterion, hint=hint)
    return pred.filtered

def filter_strs_by_index(strings, criterion, hint="None."):
    filterer = FilterStrsByIndex()
    pred = filterer(strings=strings, criterion=criterion, hint=hint)
    return pred.filtered
