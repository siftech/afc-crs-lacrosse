import dspy
from typing import Dict, List, Optional, Tuple, Any
from custom_types import FileRepair, Vulnerability, PatchWithFeedback

class CodeInput():
    code: str = dspy.InputField(desc="A file of code.")

class CodeFilepathInput():
    code_filepath: str = dspy.InputField(desc="The relative filepath of the code.")

class BuggyCodeInput():
    buggy_code: str = dspy.InputField(desc="A file of code containing some security vulnerability.")

class NumberedCodeInput():
    numbered_code: str = dspy.InputField(desc="A file of code with line numbers.")

class NumberedLogLinesInput():
    numbered_log_lines: str = dspy.InputField(desc="Contents of a log file with line numbers.")

class ProjectNameInput():
    project_name: str = dspy.InputField(desc="The name of the project whose code base contains a vulnerability.")

class SanitizerStderrInput():
    sanitizer_stderr: str = dspy.InputField(desc="The error output of a sanitizer that crashed on the vulnerability in the code.")

class SanitizerStderrOptionalInput():
    sanitizer_stderr: Optional[str] = dspy.InputField(desc="The error output of a sanitizer that crashed on the vulnerability in the code, if available.")
    
class SanitizerListOptionalInput():
    sanitizer_list: Optional[List[str]] = dspy.InputField(desc="A list of relevant sanitizers, whose union restricts the space of relevant vulnerabilities (the vulnerability must be detectable by one of them).")

class BlobInput():
    blob: str = dspy.InputField(desc="The proof-of-vulnerability (PoV) blob of input(s) that led to the crash exposed by the sanitizer, via a harness.")

class BlobOptionalInput():
    blob: Optional[str] = dspy.InputField(desc="The proof-of-vulnerability (PoV) blob of input(s) that led to the crash exposed by the sanitizer, via a harness, if available.")

class HarnessInput():
    harness: str = dspy.InputField(desc="The harness used with the proof-of-vulnerability (PoV) blob of input(s) that led to the crash exposed by the sanitizer.")

class HarnessOptionalInput():
    harness: Optional[str] = dspy.InputField(desc="The harness used with the proof-of-vulnerability (PoV) blob of input(s) that led to the crash exposed by the sanitizer, if available.")

class HarnessNameOptionalInput():
    harness_name: Optional[str] = dspy.InputField(desc="The name of the harness used with the proof-of-vulnerability (PoV) blob of input(s) that led to the crash exposed by the sanitizer, if available.")
    
class BuggyCommitDeltaInput():
    buggy_commit_delta: str = dspy.InputField(desc="The diff of the commit that introduced or exposed the vulnerability. The delta may directly contain a vulnerability, or it may expose a vulnerability elsewhere in the code.")

class BuggyCommitDeltaOptionalInput():
    buggy_commit_delta: Optional[str] = dspy.InputField(desc="The diff of the commit that introduced or exposed the vulnerability. The delta may directly contain a vulnerability, or it may expose a vulnerability elsewhere in the code, if available.")

class HintsInput():
    hints: str = dspy.InputField(desc="Extra guidance, if any.")

class HintsOptionalInput():
    hints: Optional[str] = dspy.InputField(desc="Extra guidance, if any.")

class FunctionDefinitionsInput():
    function_definitions: List[str] = dspy.InputField(desc="A list of function definitions from the same code base, some or all of which need to be modified as part of fixing a security vulnerability.")

# FIXME: Over-specified. This description probably has too much leakage from the context in which we use it, which makes it less useful as a mixin.
class CodeSnippetsInput():
    code_snippets: List[str] = dspy.InputField(desc="A list of code snippets from the same code base, some or all of which need to be modified as part of fixing a security vulnerability.")

class CodeSnippetsWithLineNumbersInput():
    code_snippets_with_line_numbers: List[str] = dspy.InputField(desc="A list of code snippets with their original line numbers.")
    
class VulnerabilityDescriptionInput():
    vulnerability_description: str = dspy.InputField(desc="A description of a cyber security vulnerability.")

class VulnerabilitiesInput():
    vulnerabilities: List[Vulnerability] = dspy.InputField(desc="Cyber security vulnerabilities.")

class VulnerabilityInput():
    vulnerability: Vulnerability = dspy.InputField(desc="The cyber security vulnerability.")
    
class ExtraCodeNotesInput():
    extra_code_notes: str = dspy.InputField(desc="Extra notes about the code, if any.")

class FileRepairInput():
    file_repair: FileRepair = dspy.InputField(desc="A planned file repair.")

class GenerationInstructionsInput():
    generation_instructions: str = dspy.InputField(desc="Instructions for how to generate the specified output.")

class DeltaFunctionBodyMapInput():
    delta_function_body_map: Dict[str, List[str]] = dspy.InputField(desc="A dict, extracted from a unified diff, whose keys are the filepaths of modified codefiles and whose values are lists of the current bodies of functions from that file which were modified by the diff.")

class PrioritizedVulnerabilitiesOutput():
    prioritized_vulnerabilities: List[Vulnerability] = dspy.OutputField(desc="A prioritized list of exploitable vulnerabilities.")

class RepairedFunctionDefinitionsOutput():
    repaired_function_definitions: List[str] = dspy.OutputField(desc="A list of the repaired versions of the input functions, i.e. all have been modified such that the code file they came from no longer contains the vulnerability.")

class RepairedCodeSnippetsOutput():
    repaired_code_snippets: List[str] = dspy.OutputField(desc="A list of the repaired versions of the input code snippets, i.e. all have been modified such that the code file they came from no longer contains the vulnerability.")

class BadPatchesWithFeedbackInput():
    bad_patches_with_feedback: List[PatchWithFeedback] = dspy.InputField(desc="One or more previous patching attempts that were assessed as unsatisfactory, along with the feedback as to why.")

class YesOrNoQuestionInput():
    yes_or_no_question: str = dspy.InputField(desc="The yes-or-no question.")

class YesOrNoWithReasonInput():
    yes_or_no_with_reason_question: str = dspy.InputField(desc="The yes-or-no question and why.")

class YesOrNoAnswerOutput():
    yes_or_no_answer: bool = dspy.OutputField(desc="The answer to the question, True for yes or False for no.")

class YesOrNoWithReasonOutput():
    yes_or_no_with_reason: Tuple[bool, str] = dspy.OutputField(desc="A tuple representing a yes/no answer along with a justification. The first entry, a boolean, is the answer -- True for yes or False for no -- and the second entry is a string representing the reason for the answer.")

class ReasoningOutput():
    # Revisit?
    reason_output: str = dspy.OutputField(desc="Reasoning feedback as to why.")

# We may want to try a json string input version.  Sarif is commonly in json.
class SarifInput():
    sarif: Dict[str, Any] = dspy.InputField(desc="A dict, extracted from a OASIS standard sarif report, whose contents reflect the details of a static analysis sarif report, which may or may not be correct.")

class TargetLanguageInput():
    target_language: str = dspy.InputField(desc="The only language that we want to find vulnerabilities in. Ignore vulnerabilities in files written in languages besides this one.")

