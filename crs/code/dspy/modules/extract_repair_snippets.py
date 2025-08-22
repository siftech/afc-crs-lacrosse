import dspy
from typing import List, Tuple, Union
from modules.mixins import NumberedCodeInput, HintsInput, FileRepairInput
import models
from utils import number_code_lines, current_dspy_lm
from code_scan import fix_fn_spans
from record import record
import json

class ExtractSnippetsSignature(dspy.Signature, NumberedCodeInput, FileRepairInput, HintsInput):
    """Extract one or more snippets of the code by providing their line number range (start/end). The snippets should together subsume all changes indicated by the provided file repair, with each snippet including a padding of at least a few lines at the beginning and end that won't need to be modified.  If two otherwise disjoint snippets are made overlapping by the required padding, they must be merged."""
    snippet_spans: List[Tuple[int, int]] = dspy.OutputField(desc="A list of pairs of line numbers--the start line number and end line number--for all regions of code relevant to the file repair, where the region includes padding of a few lines at the start and end.  Spans must be disjoint (they MUST NOT overlap with one another), and should be as minimal/circumscribed as possible while including all lines that need to be changed (plus the padding), and including any context that will be useful in implementing the repair. (The snippets may be used to implement the repair without the context of the whole file.)")

class SnippetExtractor(dspy.Module):
    def __init__(self, prefix="", verbose=False):
        self.verbose = verbose
        self.prefix = prefix
        self.extractor = dspy.Predict(ExtractSnippetsSignature)

    def record(self, name, content):
        model_name = current_dspy_lm()
        record(f"{self.prefix}/extract_snippets_{model_name}/{name}", content, False)

    def forward(self, code, file_repair, hints="None."):
        numbered_code = number_code_lines(code)
        codelines = code.split("\n")
        expred = self.extractor(numbered_code=numbered_code, file_repair=file_repair, hints=hints)
        snippet_spans = [span for span in expred.snippet_spans if span is not None]
        snippet_spans = [(start-1, end-1) for start, end in snippet_spans if end > start]
        # fn_spans = fix_fn_spans(codelines, fn_spans)
        snippets = ["\n".join(codelines[start:end+1]) for start, end in snippet_spans]
        self.record("numbered_code", numbered_code)
        self.record("snippet_spans", json.dumps(snippet_spans, indent=4))
        self.record("extracted_snippets", "\n=====================\n".join(snippets))
        return dspy.Prediction(snippets=snippets)

@models.with_escalation([models.gpt4o_1000, models.sonnet37, models.gpt41, models.sonnet4, models.gemini25pro]) # Can go small because we're just asking it for pairs of line numbers
def extract_repair_snippets(code, file_repair, hints="None.", prefix="extract_repair_snippets"):
    print("extract repair snippets")
    extractor = SnippetExtractor(prefix=prefix)
    pred = extractor(code=code, file_repair=file_repair, hints=hints)
    return pred.snippets
