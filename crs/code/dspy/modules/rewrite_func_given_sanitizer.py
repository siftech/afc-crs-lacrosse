import dspy
from typing import List
from modules.mixins import BuggyCodeInput, NumberedCodeInput, SanitizerStderrInput, HintsInput
from modules.extract_functions import extract_functions
from modules.map_strs import map_strs_gpt4o
from modules.filter_strs_by_index import filter_strs_by_index_gpt4o
# Don't rely on the shared dspyx library for now.
#from dspyx.mixins import HintsInput
#from dspyx.map_strs import map_strs_gpt4o
#from dspyx.filter_strs_by_index import filter_strs_by_index_gpt4o
from utils import replace_substrings, current_dspy_lm
from record import record
import json

class SelectFuncsGivenSanitizerSignature(dspy.Signature, BuggyCodeInput, SanitizerStderrInput, HintsInput):
    """Acting as an expert cyber security researcher, select the functions from the code whose definitions need to be modified such that the code no longer contains the vulnerability, and justify your selection."""
    function_names: List[str] = dspy.OutputField(desc="The complete list of the names of the functions whose definitions should be modified in the code file to fix the security vulnerability. Limit your selection to only functions defined in the code.")
    justification: str = dspy.OutputField(desc="The reason these functions were chosen, in the context of a repair strategy.")

class RewriteFuncsSignature(dspy.Signature, SanitizerStderrInput):
    """Acting as an expert cyber security researcher, and using the repair_strategy note describing why these functions were chosen for repair (from a longer file), rewrite the function definitions to no longer contain the vulnerability that led to the crash revealed by the sanitizer stderr output."""
    functions: List[str] = dspy.InputField(desc="A list of function definitions that probably all need to be modified to fix a security vulnerability in a code file that they were all extracted from.")
    repair_strategy: str = dspy.InputField(desc="A justification for why these functions were chosen for repair, in the context of a strategy for fixing the vulnerability.")
    repaired_functions: List[str] = dspy.OutputField(desc="A list of the repaired versions of the input functions, i.e. all have been modified such that the code file they came from no longer contains the vulnerability.")

class RewriteFuncGivenSanitizer(dspy.Module):
    def __init__(self, verbose=False):
        self.verbose = verbose
        self.selector = dspy.Predict(SelectFuncsGivenSanitizerSignature)
        self.rewriter = dspy.Predict(RewriteFuncsSignature)

    def name(self):
        model_name = current_dspy_lm()
        return f"rewrite_func_given_sanitizer_{model_name}"
        
    def record(self, record_name, content):
        record(f"{self.name()}/{record_name}", content, False)
        
    def forward(self, buggy_code, sanitizer_stderr, hints="None."):
        self.record("original_code", buggy_code)
        selpred = self.selector(buggy_code=buggy_code, sanitizer_stderr=sanitizer_stderr, hints=hints)
        function_names = selpred.function_names
        print("functions to modify:", function_names)
        selection_justification = selpred.justification
        print("justification length:", len(selection_justification))
        self.record("fn_names", json.dumps(function_names, indent=4))
        self.record("justification", selection_justification)

        fn_defs = extract_functions(buggy_code, function_names, prefix=self.name())
        
        print("fn_defs length:", len(fn_defs))
        repred = self.rewriter(functions=fn_defs, repair_strategy=selection_justification, sanitizer_stderr=sanitizer_stderr)

        original_fn_names = map_strs_gpt4o(fn_defs, "The name of the function whose definition is shown.")
        self.record("original_fn_names", json.dumps(original_fn_names, indent=4))
        repaired_fns = filter_strs_by_index_gpt4o(repred.repaired_functions, f"Functions whose names are in the list: {original_fn_names}.")
        print("repaired_functions length:", len(repaired_fns))
        self.record("repaired_functions", "\n=================\n".join(repaired_fns))
        fixed_code = replace_substrings(buggy_code, fn_defs, repaired_fns)
        print("fixed_code length:", len(fixed_code))
        self.record("fixed_code", fixed_code)
        return dspy.Prediction(fixed_code=fixed_code)

def rewrite_func_given_sanitizer(buggy_code, sanitizer_stderr, hints="None."):
    print("rewrite_func_given_sanitizer")
    rewriter = RewriteFuncGivenSanitizer()
    pred = rewriter(buggy_code=buggy_code, sanitizer_stderr=sanitizer_stderr, hints=hints)
    return pred.fixed_code
