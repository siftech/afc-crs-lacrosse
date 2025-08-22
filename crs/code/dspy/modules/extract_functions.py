import dspy
from typing import List, Tuple, Union
from modules.mixins import NumberedCodeInput, HintsInput
import models
from utils import number_code_lines, current_dspy_lm
from code_scan import fix_fn_spans
from record import record
import json

class ExtractFuncsSignature(dspy.Signature, NumberedCodeInput, HintsInput):
    """Extract the definitions of the functions exactly as each exists in the code. Pay close attention to the hints!"""
    select_function_names: List[str] = dspy.InputField(desc="The names of the functions to be extracted from the code.")
    function_spans: List[Tuple[int, int]] = dspy.OutputField(desc="A list of pairs of line numbers--the start line number and end line number--for all function defintions (including header and body) for all the select function names listed, except those whose definitions cannot be found in the code.")

class FunctionExtractor(dspy.Module):
    def __init__(self, prefix="", verbose=False):
        self.verbose = verbose
        self.prefix = prefix
        self.extractor = dspy.Predict(ExtractFuncsSignature)

    def record(self, name, content):
        model_name = current_dspy_lm()
        record(f"{self.prefix}/extract_fns_{model_name}/{name}", content, False)

    def forward(self, code, function_names, hints="None.", output_numbered=False):
        # Add numbered_codelines for if output_numbered == True
        numbered_code = number_code_lines(code)
        codelines = code.split("\n")
        numbered_codelines = numbered_code.split("\n")

        ext_hints = "The function definitions must end late enough to stand alone, e.g. include closing brackets. They also MUST start early enough to include any relevant preceding lines, e.g. type specifications"
        expred = self.extractor(numbered_code=numbered_code, select_function_names=function_names, hints=ext_hints)
        fn_spans = [fn_span for fn_span in expred.function_spans if fn_span is not None]
        fn_spans = [(start-1, end-1) for start, end in fn_spans if end > start]
        fn_spans = fix_fn_spans(codelines, fn_spans)

        if output_numbered:
            fn_defs = ["\n".join(numbered_codelines[start:end+1]) for start, end in fn_spans]
        else:
            fn_defs = ["\n".join(codelines[start:end+1]) for start, end in fn_spans]

        self.record("numbered_code", numbered_code)
        self.record("fn_spans", json.dumps(fn_spans, indent=4))
        self.record("extracted_fns", "\n=====================\n".join(fn_defs))
        return dspy.Prediction(functions=fn_defs)

@models.with_escalation([models.gpt4o_1000, models.sonnet37, models.gpt41, models.sonnet4, models.gemini25pro]) # Can go small because we're just asking it for pairs of line numbers
def extract_functions(code, function_names, hints="None.", prefix="extract_functions", output_numbered=False):
    print("extract functions")
    extractor = FunctionExtractor(prefix=prefix)
    pred = extractor(code=code, function_names=function_names, hints=hints, output_numbered=output_numbered)
    return pred.functions
