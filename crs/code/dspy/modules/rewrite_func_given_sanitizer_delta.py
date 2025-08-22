import dspy
from modules.mixins import BuggyCommitDeltaInput
from modules.rewrite_func_given_sanitizer import SelectFuncsGivenSanitizerSignature, RewriteFuncsSignature
from modules.extract_functions import extract_functions
from modules.map_strs import map_strs_gpt4o
from modules.filter_strs_by_index import filter_strs_by_index_gpt4o
from utils import replace_substrings, current_dspy_lm
from record import record
import json

class SelectFuncsGivenSanitizerDeltaSignature(SelectFuncsGivenSanitizerSignature, BuggyCommitDeltaInput):
    """Acting as an expert cyber security researcher, select the functions from the code whose definitions need to be modified such that the code no longer contains the vulnerability, and justify your selection. Note that the fix should remove the vulnerability while attempting to maintain the intended functionality of the commit."""

class RewriteFuncsDeltaSignature(RewriteFuncsSignature, BuggyCommitDeltaInput):
    """Acting as an expert cyber security researcher, and using the repair_strategy note describing why these functions were chosen for repair (from a longer file), rewrite the function definitions to no longer contain the vulnerability that led to the crash revealed by the sanitizer stderr output and the diff (delta) of the commit that introduced or exposed the vulnerability. Note that the fix should remove the vulnerability while attempting to maintain the intended functionality of the commit."""

class RewriteFuncGivenSanitizerDelta(dspy.Module):
    def __init__(self, verbose=False):
        self.verbose = verbose
        self.selector = dspy.Predict(SelectFuncsGivenSanitizerDeltaSignature)
        self.rewriter = dspy.Predict(RewriteFuncsDeltaSignature)

    def name(self):
        model_name = current_dspy_lm()
        return f"rewrite_func_given_sanitizer_delta_{model_name}"
        
    def record(self, record_name, content):
        record(f"{self.name()}/{record_name}", content, False)
        
    def forward(self, buggy_code, sanitizer_stderr, delta, hints="None."):
        self.record("original_code", buggy_code)
        selpred = self.selector(buggy_code=buggy_code, sanitizer_stderr=sanitizer_stderr, buggy_commit_delta=delta, hints=hints)
        function_names = selpred.function_names
        print("functions to modify:", function_names)
        selection_justification = selpred.justification
        print("justification length:", len(selection_justification))
        self.record("fn_names", json.dumps(function_names, indent=4))
        self.record("justification", selection_justification)

        # Refine the justification to pertain to the file
        # justification = self.strat_refiner(strategy=selection_justification)

        fn_defs = extract_functions(buggy_code, function_names, prefix=self.name())
        
        print("fn_defs length:", len(fn_defs))
        self.record("original_functions", "\n=================\n".join(fn_defs))
        repred = self.rewriter(functions=fn_defs, repair_strategy=selection_justification, sanitizer_stderr=sanitizer_stderr, buggy_commit_delta=delta, hints=hints)
        original_fn_names = map_strs_gpt4o(fn_defs, "The name of the function whose definition is shown.")
        self.record("original_fn_names", json.dumps(original_fn_names, indent=4))
        repaired_fns = filter_strs_by_index_gpt4o(repred.repaired_functions, f"Functions whose names are in the list: {original_fn_names}.")
        print("repaired_functions length:", len(repaired_fns))
        self.record("repaired_functions", "\n=================\n".join(repaired_fns))
        fixed_code = replace_substrings(buggy_code, fn_defs, repaired_fns)
        print("fixed_code length:", len(fixed_code))
        self.record("fixed_code", fixed_code)
        return dspy.Prediction(fixed_code=fixed_code)

rewriter = RewriteFuncGivenSanitizerDelta()

def rewrite_func_given_sanitizer_delta(buggy_code, sanitizer_stderr, delta, hints="None."):
    print("rewrite_func_given_sanitizer_delta")
    pred = rewriter(buggy_code=buggy_code, sanitizer_stderr=sanitizer_stderr, delta=delta, hints=hints)
    return pred.fixed_code
