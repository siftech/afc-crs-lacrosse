import os
import json
import dspy
import models as models
from typing import List
from utils import read_file, well_formed_delta, well_formed_blob, well_formed_sanitizer_stderr, replace_substrings, current_dspy_lm
from modules.extract_functions import extract_functions
from modules.extract_repair_snippets import extract_repair_snippets
from modules.map_strs import map_strs_gpt4o
from modules.filter_strs_by_index import filter_strs_by_index_gpt4o
from modules.mixins import FunctionDefinitionsInput, FileRepairInput, VulnerabilityDescriptionInput, ExtraCodeNotesInput, SanitizerStderrInput, HintsInput, BuggyCommitDeltaInput, BlobInput, RepairedFunctionDefinitionsOutput, CodeSnippetsInput, RepairedCodeSnippetsOutput, SanitizerStderrOptionalInput, BuggyCommitDeltaOptionalInput, BlobOptionalInput
from modules.ask_yes_no_about_file_repair import ask_yes_no_about_file_repair
from record import record

# Base class
class RewriteFnsSignature(dspy.Signature, FunctionDefinitionsInput, FileRepairInput, VulnerabilityDescriptionInput, ExtraCodeNotesInput, HintsInput, RepairedFunctionDefinitionsOutput):
    """Acting as an expert cyber security researcher, implement the file repair by rewriting the provided function definitions."""

# We currently handle five combinations of inputs:

# sanitizer only
class RewriteFnsGivenSanitizerSignature(RewriteFnsSignature, SanitizerStderrInput):
    """Acting as an expert cyber security researcher, implement the file repair by rewriting the provided function definitions. For reference, you are also provided the vulnerability description (which led to this file repair and potentially others), extra notes about the code, and the stderr of the memory sanitizer crash that revealed the bug."""

# delta only
class RewriteFnsGivenDeltaSignature(RewriteFnsSignature, BuggyCommitDeltaInput):
    """Acting as an expert cyber security researcher, implement the file repair by rewriting the provided function definitions. For reference, you are also provided the vulnerability description (which led to this file repair and potentially others), extra notes about the code, and the diff of the commit that introduced or exposed the bug."""

# sanitizer and blob
class RewriteFnsGivenSanitizerBlobSignature(RewriteFnsGivenSanitizerSignature, BlobInput):
    """Acting as an expert cyber security researcher, implement the file repair by rewriting the provided function definitions. For reference, you are also provided the vulnerability description (which led to this file repair and potentially others), extra notes about the code, the stderr of the memory sanitizer crash that revealed the bug, and the crash-inducing input blob."""

# sanitizer and delta
class RewriteFnsGivenSanitizerDeltaSignature(RewriteFnsGivenSanitizerSignature, BuggyCommitDeltaInput):
    """Acting as an expert cyber security researcher, implement the file repair by rewriting the provided function definitions. For reference, you are also provided the vulnerability description (which led to this file repair and potentially others), extra notes about the code, the stderr of the memory sanitizer crash that revealed the bug, and the diff of the commit that introduced or exposed the bug."""

# sanitizer, blob, and delta
class RewriteFnsGivenSanitizerBlobDeltaSignature(RewriteFnsGivenSanitizerBlobSignature, BuggyCommitDeltaInput):
    """Acting as an expert cyber security researcher, implement the file repair by rewriting the provided function definitions. For reference, you are also provided the vulnerability description (which led to this file repair and potentially others), extra notes about the code, the stderr of the memory sanitizer crash that revealed the bug, the crash-inducing input blob, and the diff of the commit that introduced or exposed the bug."""

# Trying a new pattern for this signature, where we just make one signature with some inputs as optional
class RewriteSnippetsSignature(dspy.Signature, CodeSnippetsInput, FileRepairInput, VulnerabilityDescriptionInput, ExtraCodeNotesInput, HintsInput, SanitizerStderrOptionalInput, BuggyCommitDeltaOptionalInput, BlobOptionalInput, RepairedCodeSnippetsOutput):
    """Acting as an expert cyber security researcher, implement the file repair by rewriting the provided code snippets. The list of rewritten snippets should have the same length as the list of input snippets, and each rewritten snippet should be able to replace the corresponding input snippet (starting and ending at corresponding lines of code -- not necessarily in terms of line numbers but in terms of functionality).  You may receive additional details about the true vulnerability that the vulnerability hypotheses are trying to address."""
    
class ImplementFileRepair(dspy.Module):
    def __init__(self, verbose=False):
        self.verbose = verbose
        self.fn_rewriter_san_blob_delta = dspy.Predict(RewriteFnsGivenSanitizerBlobDeltaSignature)
        self.fn_rewriter_san_delta = dspy.Predict(RewriteFnsGivenSanitizerDeltaSignature)
        self.fn_rewriter_san_blob = dspy.Predict(RewriteFnsGivenSanitizerBlobSignature)
        self.fn_rewriter_san = dspy.Predict(RewriteFnsGivenSanitizerSignature)
        self.fn_rewriter_delta = dspy.Predict(RewriteFnsGivenDeltaSignature)
        self.fn_rewriter_base = dspy.Predict(RewriteFnsSignature)
        self.snippet_rewriter = dspy.Predict(RewriteSnippetsSignature)

    def name(self):
        model_name = current_dspy_lm()
        return f"implement_file_repair_{model_name}"
        
    def record(self, record_name, content):
        record(f"{self.name()}/{record_name}", content, False)

    def file_repair_requires_snippets(self, file_repair, file_code):
        if len(file_repair.target_function_names) == 0:
            return True
        yn_question = "Does the file repair include any changes that occur outside of a function or method, i.e., at the top level in C or at the class level in Java?"
        global_scope_p = ask_yes_no_about_file_repair(yn_question, file_repair, file_code)
        return global_scope_p

    def forward(self, file_repair, file_code, vuln, codebase_path, sanitizer_stderr=None, pov_blob=None, delta=None, pov_harness=None, hints="None."):
        print("implement_file_repair forward method")
        self.record("file_repair", file_repair)
        self.record("original_code", file_code)

        vuln_desc = vuln.description
        self.record("vuln_desc", vuln_desc)
        vuln_notes = vuln.notes
        self.record("vuln_notes", vuln_notes)
        
        fn_names = file_repair.target_function_names
        self.record("fn_names", fn_names)

        self.record("rewriter_hints", hints)
        
        if self.file_repair_requires_snippets(file_repair, file_code):
            print("Use snippets (instead of functions/methods).")
            snippets = extract_repair_snippets(file_code, file_repair, prefix=self.name())
            for i, snippet in enumerate(snippets):
                print(f"Snippet:")
                print(snippet)
            self.record("original_snippets", "\n=================\n".join(snippets))
            repred = self.snippet_rewriter(code_snippets=snippets, file_repair=file_repair, vulnerability_description=vuln_desc, extra_code_notes=vuln_notes, sanitizer_stderr=sanitizer_stderr, blob=pov_blob, buggy_commit_delta=delta, hints=hints)
            new_snippets = repred.repaired_code_snippets
            repaired_code = replace_substrings(file_code, snippets, new_snippets)

        else:
            print("Use functions/methods (instead of snippets).")
            fn_defs = extract_functions(file_code, fn_names, prefix=self.name())
            # self.record("original_fn_defs", fn_defs)
            self.record("original_functions", "\n=================\n".join(fn_defs))
            print("fn_defs length:", len(fn_defs))
            original_fn_names = map_strs_gpt4o(fn_defs, "The name of the function whose definition is shown.")
            self.record("original_fn_names", json.dumps(original_fn_names, indent=4))
        
            # Repair the FTP by selecting and rewriting individual functions.
            # Here's where we dispatch to the different prompts, depending on what inputs we have.
            wf_san = well_formed_sanitizer_stderr(sanitizer_stderr)
            wf_blob = well_formed_blob(pov_blob)
            wf_delta = well_formed_delta(delta)
            if wf_san and wf_blob and wf_delta:
                print("implement_file_repair rewriter uses: san, blob, delta")
                repred = self.fn_rewriter_san_blob_delta(function_definitions=fn_defs, file_repair=file_repair, vulnerability_description=vuln_desc, extra_code_notes=vuln_notes, sanitizer_stderr=sanitizer_stderr, blob=pov_blob, buggy_commit_delta=delta, hints=hints)
            elif wf_san and wf_delta:
                print("implement_file_repair rewriter uses: san, delta")
                repred = self.fn_rewriter_san_delta(function_definitions=fn_defs, file_repair=file_repair, vulnerability_description=vuln_desc, extra_code_notes=vuln_notes, sanitizer_stderr=sanitizer_stderr, buggy_commit_delta=delta, hints=hints)
            elif wf_san and wf_blob:
                print("implement_file_repair rewriter uses: san, blob")
                repred = self.fn_rewriter_san_blob(function_definitions=fn_defs, file_repair=file_repair, vulnerability_description=vuln_desc, extra_code_notes=vuln_notes, sanitizer_stderr=sanitizer_stderr, blob=pov_blob, hints=hints)
            elif wf_san:
                print("implement_file_repair rewriter uses: san")
                repred = self.fn_rewriter_san(function_definitions=fn_defs, file_repair=file_repair, vulnerability_description=vuln_desc, extra_code_notes=vuln_notes, sanitizer_stderr=sanitizer_stderr, hints=hints)
            elif wf_delta:
                print("implement_file_repair rewriter uses: delta")
                repred = self.fn_rewriter_delta(function_definitions=fn_defs, file_repair=file_repair, vulnerability_description=vuln_desc, extra_code_notes=vuln_notes, buggy_commit_delta=delta, hints=hints)
            else:
                print("implement_file_repair rewriter uses: no additional inputs (base)")
                repred = self.fn_rewriter_base(function_definitions=fn_defs, file_repair=file_repair, vulnerability_description=vuln_desc, extra_code_notes=vuln_notes, hints=hints)

            repaired_function_definitions = repred.repaired_function_definitions
            self.record("repaired_function_definitions", repaired_function_definitions)

            repaired_fns = filter_strs_by_index_gpt4o(repaired_function_definitions, f"Functions whose names are in the list: {original_fn_names}.")
            print("repaired_functions length:", len(repaired_fns))
            self.record("repaired_functions", "\n=================\n".join(repaired_fns))
            repaired_code = replace_substrings(file_code, fn_defs, repaired_fns)
        
        print("original code length:", len(file_code))
        print("repaired code length:", len(repaired_code))
        self.record("repaired_code", repaired_code)
        return dspy.Prediction(repaired_code=repaired_code)

def implement_file_repair(file_repair, file_code, vuln, codebase_path, sanitizer_stderr=None, pov_blob=None, delta=None, pov_harness=None, hints="None."):
    print("implement_file_repair")
    implementer = ImplementFileRepair()
    pred = implementer(file_repair=file_repair, file_code=file_code, vuln=vuln, codebase_path=codebase_path, sanitizer_stderr=sanitizer_stderr, pov_blob=pov_blob, delta=delta, pov_harness=pov_harness, hints=hints)
    return pred.repaired_code
