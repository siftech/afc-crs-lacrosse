import dspy
import models as models
# from caching import cache_predictions
from typing import List, Optional, Dict, Tuple

class ParseSanitizerSignature(dspy.Signature):
    """Parse the output of the address sanitizer to extract the line number in the source file associated with the crash."""
    sanitizer_output: str = dspy.InputField(desc="The output of the address sanitizer.")
    stack_trace: List[Tuple[str, str, int, int]] = dspy.OutputField(desc="A list representing the call stack when the crash occurred, starting with the top of the call stack, and ignoring the stack entries that take place within the internals of the fuzzer (these should be lower in the stack). Each entry in the list is a tuple with four items: (1) the name of the function, (2) the file path containing the function, (3) the integer representing the line number, and (4) the integer representing the character number measured from line start.")
    harness_call: Tuple[str, str, int, int] = dspy.OutputField(desc="The call to the harness, used by the fuzzer (typically the next call in the stack after the calls that are not within the fuzzer). Capture (1) the name of the function, (2) the file path containing the function, (3) the integer representing the line number, and (4) the integer representing the character number measured from line start.")
    bug_summary: str = dspy.OutputField(desc="A brief summary of the nature of the bug.")
    bug_hint: Optional[str] = dspy.OutputField(desc="The hint(s), if any, given by the sanitizer about the nature of the bug.")
    
# @cache_predictions(use_cache=True)
class ParseSanitizer(dspy.Module):
    def __init__(self, verbose=False):
        self.verbose = verbose
        self.parser = dspy.Predict(ParseSanitizerSignature)
        
    def forward(self, sanitizer_output):
        pred = self.parser(sanitizer_output=sanitizer_output)
        return dspy.Prediction(stack_trace=pred.stack_trace, harness_call=pred.harness_call, bug_summary=pred.bug_summary, bug_hint=pred.bug_hint)
        
parser = ParseSanitizer()

# Try it with more and more competent models until one can get the format right
@models.with_escalation([models.gpt4omini_1000, models.gpt4o_1000, models.sonnet37, models.gpt41, models.sonnet4, models.gemini25pro])
def parse_sanitizer_output(sanitizer_output):
    pred = parser(sanitizer_output=sanitizer_output)
    return pred.stack_trace, pred.harness_call, pred.bug_summary, pred.bug_hint
        
