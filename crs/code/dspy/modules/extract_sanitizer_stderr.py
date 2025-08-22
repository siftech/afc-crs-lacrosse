import dspy
from typing import List, Tuple, Union, Optional
from modules.mixins import NumberedLogLinesInput, ProjectNameInput, HintsInput
from modules.ask_yes_no import ask_yes_no
import models
from utils import number_code_lines, current_dspy_lm
from code_scan import fix_fn_spans
from record import record
import json

class ExtractSanitizerStderrSignature(dspy.Signature, NumberedLogLinesInput, ProjectNameInput, HintsInput):
    """Acting as an expert cyber security researcher, extract a snippet from the log that includes the sanitizer stderr (generated using a sanitizer). The snippet should be specified by providing a pair of line numbers for its start and end."""
    sanitizer_stderr_span: Tuple[int, int] = dspy.OutputField(desc="A pair of line numbers--the start line number and end line number--for the region of the input (lines from a log file) that is as minimal/circumscribed as possible while including the entire sanitizer stderr (including one or more stack traces) for the crash.")

class ExtractSanitizerStderrWithProjectInstrumentationSignature(dspy.Signature, NumberedLogLinesInput, ProjectNameInput, HintsInput):
    """Acting as an expert cyber security researcher, extract two types of snippets from the log-- (1) the sanitizer stderr snippet, and (2) snippets containing instrumentation of project-specific classes (for the project name provided). The snippets should be specified by providing, for each, a line span, i.e. a pair of line numbers designating its starting and ending line number."""
    project_instrumentation_spans: List[Tuple[int, int]] = dspy.OutputField(desc="A list of pairs of line numbers--the start line number and end line number--for any and all regions of the input (lines from a log file) that log the instrumentation of *project-specific* classes, where the project is the one whose name is provided.")
    sanitizer_stderr_span: Tuple[int, int] = dspy.OutputField(desc="A pair of line numbers--the start line number and end line number--for the region of the input (lines from a log file) that constitutes the entire the sanitizer stderr.")

class SanitizerStderrExtractor(dspy.Module):
    def __init__(self, verbose=False):
        self.verbose = verbose
        self.extractor = dspy.Predict(ExtractSanitizerStderrSignature)
        self.extractor_w_inst = dspy.Predict(ExtractSanitizerStderrWithProjectInstrumentationSignature)

    def record(self, name, content):
        model_name = current_dspy_lm()
        if self.extract_instrumentation:
            record(f"extract_sanitizer_stderr_w_instrumentation_{model_name}/{name}", content, True)
        else:
            record(f"extract_sanitizer_stderr_{model_name}/{name}", content, True)
            
    def forward(self, log_contents, project_name=None, hints="None."):
        inst_yn_question = f"""The log shown below is from a script that reproduced a sanitizer crash for a vulnerability found while fuzzing a project. Outside of the sanitizer stderr, does the log include project-relevant instrumentation information that might help hunt down the vulnerability?
        Log contents:
        {log_contents}
        """
        self.extract_instrumentation = ask_yes_no(inst_yn_question)

        if self.extract_instrumentation:
            assert project_name is not None, "When extract_instrumentation is True, a project_name must be provided and it can't be None."
        
        numbered_log_lines = number_code_lines(log_contents)
        loglines = log_contents.split("\n")
        self.record("numbered_log_lines", numbered_log_lines)
        
        def safe_extract(start, end):
            # correct for one-indexing
            start = start - 1
            end = end - 1
            return "\n".join(loglines[max(0, start) : min(len(loglines), end + 1)]) # don't go out of bounds

        project_instrumentation_sections = None
        if self.extract_instrumentation:
            print("also extracting project-related instrumentation")
            expred = self.extractor_w_inst(numbered_log_lines=numbered_log_lines, project_name=project_name, hints=hints)
            # should be ascending order
            inst_spans = sorted(expred.project_instrumentation_spans, key=lambda x: x[-1])
            self.record("project_instrumentation_spans", json.dumps(inst_spans, indent=4))
            inst_sections = [safe_extract(inst_start, inst_end) for inst_start, inst_end in inst_spans]
            self.record("project_instrumentaton_sections", project_instrumentation_sections)
        else:
            print("not including any instrumentation")
            expred = self.extractor(numbered_log_lines=numbered_log_lines, hints=hints)
            
        sani_start, sani_end = expred.sanitizer_stderr_span
        sanitizer_stderr = safe_extract(sani_start, sani_end)
        self.record("sanitizer_stderr_span", json.dumps(expred.sanitizer_stderr_span, indent=4))
        self.record("extracted_sanitizer_stderr", sanitizer_stderr)

        if self.extract_instrumentation and (len(inst_spans) > 0):
            if inst_spans[-1][-1] == (sani_start - 1): # smush the last inst into the stderr if adjacent
                sanitizer_stderr = f"{inst_sections[-1]}\n{sanitizer_stderr}"
                inst_sections.pop() # remove the last inst item if it got smushed
            sanitizer_stderr = "\n[...]\n".join(inst_sections + [sanitizer_stderr])
            
        self.record("extracted_sanitizer_stderr_w_inst", sanitizer_stderr)
        
        return dspy.Prediction(sanitizer_stderr=sanitizer_stderr)

@models.with_escalation([models.gpt4o_1000, models.sonnet37, models.gpt41, models.sonnet4, models.gemini25pro]) # Can go small because we're just asking it for pairs of line numbers
def extract_sanitizer_stderr_with_instrumentation(log_contents, project_name=None, hints="None."):
    print("extract_sanitizer_stderr_with_instrumentation")
    extractor = SanitizerStderrExtractor()
    pred = extractor(log_contents=log_contents, project_name=project_name, hints=hints)
    return pred.sanitizer_stderr
