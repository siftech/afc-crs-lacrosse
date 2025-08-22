import dspy
# from typing import List
from modules.mixins import BuggyCodeInput, SanitizerInput, HintsInput

class RewriteCodeGivenSanitizerSignature(BuggyCodeInput, SanitizerInput, HintsInput):
    """Acting as an expert cyber security researcher, rewrite the code file to no longer contain the vulnerability that has been revealed by the sanitizer."""
    fixed_code: str = dspy.OutputField(desc="A rewritten version of the code file, with the security vulnerability fixed, and no unrelated changes. All whitespace should mimic the buggy version of the code.")

class RewriteCodeGivenSanitizer(dspy.Module):
    def __init__(self, verbose=False):
        self.verbose = verbose
        self.rewriter = dspy.Predict(RewriteCodeGivenSanitizerSignature)
        
    def forward(self, buggy_code, sanitizer_stderr, hints="None."):
        pred = self.rewriter(buggy_code=buggy_code, sanitizer_stderr=sanitizer_stderr, hints=hints)
        return dspy.Prediction(fixed_code=pred.fixed_code)

rewriter = RewriteCodeGivenSanitizer()

def rewrite_code_given_sanitizer(buggy_code, sanitizer_stderr, hints="None."):
    pred = rewriter(buggy_code=buggy_code, sanitizer_stderr=sanitizer_stderr, hints=hints)
    return pred.fixed_code
