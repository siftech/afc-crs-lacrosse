import dspy
from typing import List, Tuple, Union
from modules.mixins import NumberedLogLinesInput, HintsInput
import models
from utils import number_code_lines, current_dspy_lm
from code_scan import fix_fn_spans
from record import record
import json

class ExtractStackTraceSignature(dspy.Signature, NumberedLogLinesInput, HintsInput):
    """Acting as an expert cyber security researcher, extract a snippet from the log that includes the stack trace (generated using a sanitizer). The snippet should be specified by providing a pair of line numbers for its start and end."""
    stack_trace_span: Tuple[int, int] = dspy.OutputField(desc="A pair of line numbers--the start line number and end line number--for the region of the input (lines from a log file) that is as minimal/circumscribed as possible while including the entire stack trace (or multiple stack traces) for the crash.")

class ExtractStackTraceWithPaddingSignature(dspy.Signature, NumberedLogLinesInput, HintsInput):
    """Acting as an expert cyber security researcher, extract a snippet from the log that includes the stack trace (generated using a sanitizer), plus any other sanitizer stderr output and any other relevant lines before or after the sanitizer stderr (i.e. padding) that seems potentially helpful for understanding where in the code base the bug might be which caused the crash. The snippet should be specified by providing a pair of line numbers for its start and end."""
    stack_trace_span: Tuple[int, int] = dspy.OutputField(desc="A pair of line numbers--the start line number and end line number--for the region of the input (lines from a log file) that includes the entire stack trace (or multiple stack traces) as well as any lines before or after the stack trace that might help someone infer which file the vulnerability/bug is in which caused the crash.")

class StackTraceExtractor(dspy.Module):
    def __init__(self, prefix="", verbose=False):
        self.verbose = verbose
        self.prefix = prefix
        self.extractor = dspy.Predict(ExtractStackTraceSignature)
        self.extractor_padded = dspy.Predict(ExtractStackTraceWithPaddingSignature)

    def record(self, name, content):
        model_name = current_dspy_lm()
        record(f"extract_stack_trace_{model_name}/{name}", content, True)

    def forward(self, log_contents, include_relevant_padding=False, hints="None."):                                    
        numbered_log_lines = number_code_lines(log_contents)
        loglines = log_contents.split("\n")
        if include_relevant_padding:
            print("including relevant padding")
            expred = self.extractor_padded(numbered_log_lines=numbered_log_lines, hints=hints)
        else:
            print("not including relevant padding")
            expred = self.extractor(numbered_log_lines=numbered_log_lines, hints=hints)
        start, end = expred.stack_trace_span
        start = start - 1 # from one-indexed to zero-indexed
        end = end - 1
        # stack_trace = "\n".join(loglines[start:end+1])
        stack_trace = "\n".join(loglines[max(0, start) : min(len(loglines), end + 1)]) # don't go out of bounds
        self.record("numbered_log_lines", numbered_log_lines)
        self.record("stack_trace_span", json.dumps(expred.stack_trace_span, indent=4))
        self.record("extracted_stack_trace", stack_trace)
        self.record("include_relevant_padding", include_relevant_padding)
        return dspy.Prediction(stack_trace=stack_trace)

@models.with_escalation([models.gpt4o_1000, models.sonnet37, models.gpt41, models.sonnet4, models.gemini25pro]) # Can go small because we're just asking it for a pair of line numbers
def extract_stack_trace(log_contents, include_relevant_padding=False, hints="None."):
    print("extract_stack_trace")
    extractor = StackTraceExtractor()
    pred = extractor(log_contents=log_contents, include_relevant_padding=include_relevant_padding, hints=hints)
    return pred.stack_trace
