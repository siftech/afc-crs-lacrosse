import dspy
from modules.mixins import BlobInput
from modules.rewrite_func_given_sanitizer import SelectFuncsGivenSanitizerSignature, RewriteFuncsSignature
from modules.extract_functions import extract_functions
from modules.map_strs import map_strs_gpt4o
from modules.filter_strs_by_index import filter_strs_by_index_gpt4o
from utils import replace_substrings, current_dspy_lm
from record import record
import json

class SelectFuncsGivenSanitizerBlobSignature(SelectFuncsGivenSanitizerSignature, BlobInput):
    """Acting as an expert cyber security researcher, select the functions from the code whose definitions need to be modified such that the code no longer contains the vulnerability, and justify your selection."""
    
class RewriteFuncsBlobSignature(RewriteFuncsSignature, BlobInput):
    """Acting as an expert cyber security researcher, and using the repair_strategy note describing why these functions were chosen for repair (from a longer file), rewrite the function definitions to no longer contain the vulnerability that led to the crash revealed by the sanitizer stderr output and the bug-inducing input blob."""

class RewriteFuncGivenSanitizerBlob(dspy.Module):
    def __init__(self, verbose=False):
        self.verbose = verbose
        self.selector = dspy.Predict(SelectFuncsGivenSanitizerBlobSignature)
        self.rewriter = dspy.Predict(RewriteFuncsBlobSignature)

    def name(self):
        model_name = current_dspy_lm()
        return f"rewrite_func_given_sanitizer_blob_{model_name}"
        
    def record(self, record_name, content):
        record(f"{self.name()}/{record_name}", content, False)
        
    def forward(self, buggy_code, sanitizer_stderr, blob, hints="None."):
        self.record("original_code", buggy_code)
        selpred = self.selector(buggy_code=buggy_code, sanitizer_stderr=sanitizer_stderr, blob=blob, hints=hints)
        function_names = selpred.function_names
        print("functions to modify:", function_names)
        selection_justification = selpred.justification
        print("justification length:", len(selection_justification))
        self.record("fn_names", json.dumps(function_names, indent=4))
        self.record("justification", selection_justification)

        fn_defs = extract_functions(buggy_code, function_names, prefix=self.name())
        
        print("fn_defs length:", len(fn_defs))
        self.record("original_functions", "\n=================\n".join(fn_defs))
        repred = self.rewriter(functions=fn_defs, repair_strategy=selection_justification, sanitizer_stderr=sanitizer_stderr, blob=blob)

        original_fn_names = map_strs_gpt4o(fn_defs, "The name of the function whose definition is shown.")
        self.record("original_fn_names", json.dumps(original_fn_names, indent=4))
        repaired_fns = filter_strs_by_index_gpt4o(repred.repaired_functions, f"Functions whose names are in the list: {original_fn_names}.")
        print("repaired_functions length:", len(repaired_fns))
        self.record("repaired_functions", "\n=================\n".join(repaired_fns))
        fixed_code = replace_substrings(buggy_code, fn_defs, repaired_fns)
        print("fixed_code length:", len(fixed_code))
        self.record("fixed_code", fixed_code)
        return dspy.Prediction(fixed_code=fixed_code)

rewriter = RewriteFuncGivenSanitizerBlob()

def rewrite_func_given_sanitizer_blob(buggy_code, sanitizer_stderr, blob, hints="None."):
    print("rewrite_func_given_sanitizer_blob")
    pred = rewriter(buggy_code=buggy_code, sanitizer_stderr=sanitizer_stderr, blob=blob, hints=hints)
    return pred.fixed_code
