import dspy
import models as models
# from caching import cache_predictions
from typing import List

class ChooseStringsSignature(dspy.Signature):
    """Select the strings that satisfy the criterion by listing their line numbers."""
    strings: str = dspy.InputField(desc="Choice numbers are at the beginning of each string, followed by a period.")
    criterion: str = dspy.InputField(desc="A description of which strings to choose.")
    hint: str = dspy.InputField(desc="Tips for choosing.")
    selected: List[str] = dspy.OutputField(desc="The numbers corresponding to the strings that meet the criterion.")

class ChooseStrings(dspy.Module):
    def __init__(self, verbose=True):
        self.verbose = verbose
        self.chooser = dspy.Predict(ChooseStringsSignature)
        
    def forward(self, strings, criterion, hint="None."):
        # One-index the lines. This tends to go a lot better because the LLMs (openAI's at least) seem to want to do it anyway.
        # One-indexed line numbers are a convention AFAICT.
        numbered_code = "".join([f"{i+1}.{string}" for i, string in enumerate(strings)])
        if self.verbose:
            print()
            print("numbered code:")
            print(numbered_code)
        pred = self.chooser(strings=strings, criterion=criterion, hint=hint)
        return dspy.Prediction(chosen=[ind-1 for ind in pred.selected])

chooser = ChooseStrings()

@models.with_escalation([models.gpt4o_1000, models.sonnet37, models.gpt41, models.sonnet4, models.gemini25pro])
def choose_strings(strings, criterion, hint="None."):
    pred = chooser(strings=strings, criterion=criterion, hint=hint)
    return pred.chosen
