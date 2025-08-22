import dspy
import models as models
from typing import List, Optional

class BuggyCommitInputsMixin(dspy.Signature):
    """Parent class for signatures that take bug descriptions as input."""
    code: str = dspy.InputField(desc="The code containing the vulnerability.")
    code_start: int = dspy.InputField(desc="The line number where the buggy code starts (relative to its file).")
    code_end: int = dspy.InputField(desc="The line number where the buggy code ends (relative to its file).")
    buggy_commit_diff: str = dspy.InputField(desc="The diff for the commit that introduced the vulnerability, which is the commit immediately preceding the function definition.")

class PovInputsMixin(dspy.Signature):
    """A mixin class for PoV-related inputs."""
    vulnerability_blob: str = dspy.InputField(desc="The blob of inputs that exposed the vulnerability by inducing a crash in the sanitizer.")

class SanitizerInputsMixin(dspy.Signature):
    sanitizer_crash_summary: str = dspy.InputField(desc="A summary of the bug extracted from the sanitizer error log.")
    sanitizer_crash_hint: Optional[str] = dspy.InputField(desc="A hint about the bug provided by the sanitizer error log.")
    # [MDM] I'm not sure I trust the sanitizer to pin these down reliably.
    # crash_line: str = dspy.InputField(desc="The line in the function where the sanitizer crash occurred.")
    # crash_line_remainder: str = dspy.InputField(desc="The remainder of the line at the character where the sanitizer crash occurred.")

class DiagnoseBugSignature(BuggyCommitInputsMixin, SanitizerInputsMixin, PovInputsMixin):
    """Synthesize the bug information from the address sanitizer with the code and the diff of the buggy commit to characterize the bug by assigning a bu-type and describing the bug in terms of the variables and operations in the code."""
    guidance: str = dspy.InputField(desc="Guidance about how to assess the bug(s) in the commit.")
    bug_type: str = dspy.OutputField(desc="""Assign a bug type from the list.
    Heap Buffer Overflow: Out-of-bounds access (read/write) in dynamically allocated memory (e.g., malloc, new);
    Stack Buffer Overflow: Out-of-bounds access (read/write) in stack-allocated memory;
    Global Buffer Overflow: Out-of-bounds access (read/write) in global or static variables;
    Heap Use-After-Free: Access to memory after it has been deallocated from the heap;
    Use-After-Return: Access to stack memory after the function has returned;
    Use-After-Scope: Access to stack or local variables after their scope has ended;
    Null Pointer Dereference: Attempt to access memory at address 0x0;
    Dangling Pointer Dereference: Access to memory via a pointer that refers to a deallocated block;
    Wild Pointer Dereference: Access using an uninitialized or invalid pointer (typically shows as an access to an unexpected address);
    Out-of-Bounds Read: Reading outside allocated memory (e.g., array out-of-bounds);
    Out-of-Bounds Write: Writing outside allocated memory (e.g., writing past array boundaries);
    Double-Free: Memory deallocation attempt on a block that has already been freed;
    Invalid Free: Freeing memory that was never allocated (e.g., pointer arithmetic errors or freeing an invalid address);
    Access to Red-Zone Memory: Read or write operations in guard zones around stack, heap, or global allocations;
    Uninitialized Memory Read: Reading memory before it has been initialized (tracked by shadow memory);
    Misaligned Memory Access: Accessing memory at an address that violates alignment requirements, typically leading to crashes on certain architectures;
    Invalid Call to free(): Calling free() on a pointer not pointing to the start of a memory block or an invalid pointer;
    Invalid Call to realloc(): Reallocating memory on a pointer that is invalid or not properly allocated;
    Invalid Memory Shadow Access: Errors in memory regions protected by shadow memory due to corruption or mismanagement;
    ected Leak (Heap): Heap-allocated memory not freed before the program terminates.""")
    bug_details: str = dspy.OutputField(desc="The detailed reconstruction of how the bug could have possibly occurred, in terms of the assigned bug-type, the relevant variables and operations in the code.")

class DiagnoseBug(dspy.Module):
    def __init__(self, verbose=False):
        self.verbose = verbose
        self.diagnoser = dspy.Predict(DiagnoseBugSignature)
        
    def forward(self, code, start, end, diff, summary, hint, vuln, guidance):
        pred = self.diagnoser(code=code, code_start=start, code_end=end, buggy_commit_diff=diff, sanitizer_crash_summary=summary, sanitizer_crash_hint=hint, vulnerability_blob=vuln, guidance=guidance)
        return dspy.Prediction(bug_type=pred.bug_type, bug_details=pred.bug_details)

diagnoser = DiagnoseBug()

@models.with_escalation([models.gpt4o_10000, models.sonnet37, models.gpt41, models.sonnet4, models.gemini25pro])
def diagnose_bug(code, start, end, diff, summary, hint, vuln, guidance="None."):
    pred = diagnoser(code=code, start=start, end=end, diff=diff, summary=summary, hint=hint, vuln=vuln, guidance=guidance)
    return pred.bug_type, pred.bug_details


class CommitCritiqueSignature(BuggyCommitInputsMixin):
    """Critique the implementation of the commit, knowing that it introduced the vulnerability described."""
    bug_details: str = dspy.InputField(desc="Details about the vulnerability introduced.")
    guidance: str = dspy.InputField(desc="Guidance about how to approach the critique.")
    commit_intent: str = dspy.OutputField(desc="The new functionality added by the buggy commit, i.e. the enhancement or repair intended by its author.")
    commit_critique: str = dspy.OutputField(desc="A critique of the approach taken in the commit. Where was it wrong-headed, such that the vulnerability was introduced.")

        
class CommitCritiqueBug(dspy.Module):
    def __init__(self, verbose=False):
        self.verbose = verbose
        self.critiquer = dspy.Predict(CommitCritiqueSignature)
        
    def forward(self, code, start, end, diff, bug_details, guidance):
        pred = self.critiquer(code=code, code_start=start, code_end=end, buggy_commit_diff=diff, bug_details=bug_details, guidance=guidance)
        return dspy.Prediction(critique=pred.commit_critique, intent=pred.commit_intent)

critiquer = CommitCritiqueBug()

@models.with_escalation([models.gpt4o_10000, models.sonnet37, models.gpt41, models.sonnet4, models.gemini25pro])
def critique_commit(code, start, end, diff, bug_details, guidance="None."):
    pred = critiquer(code=code, start=start, end=end, diff=diff, bug_details=bug_details, guidance=guidance)
    return pred.intent, pred.critique


class PatchBugSignature(BuggyCommitInputsMixin):
    """Patch the function so that it no longer has the bug, without compromising the intent of the commit."""
    commit_intent: str = dspy.InputField(desc="A description of how the commit was intended to enhance the code.")
    bug_details: str = dspy.InputField(desc="A description of the unintended security vulnerability in the commit.")
    commit_critique: str = dspy.InputField(desc="A critique of where the commit went wrong in implementing its intent.")
    patched_filepath: str = dspy.InputField(desc="The relative filepath to use in the diff.")
    patch: str = dspy.OutputField(desc="A patch that will apply to the code to repair it such that the vulnerability is removed, but the intent of the commit is preserved. This will be in the form of a diff, noting the filepath, code_start line and code_end line of the repaired function.")
    
class PatchBug(dspy.Module):
    def __init__(self, verbose=False):
        self.verbose = verbose
        self.patcher = dspy.Predict(PatchBugSignature)
        
    def forward(self, code, start, end, diff, intent, bug_details, critique, filepath):
        pred = self.patcher(code=code, code_start=start, code_end=end, buggy_commit_diff=diff, commit_intent=intent, bug_details=bug_details, commit_critique=critique, patched_filepath=filepath)
        return dspy.Prediction(patch=pred.patch)

patcher = PatchBug()

@models.with_escalation([models.gpt4o_10000, models.sonnet37, models.gpt41, models.sonnet4, models.gemini25pro])
def patch_bug(code, start, end, diff, intent, bug_details, critique, filepath):
    pred = patcher(code=code, start=start, end=end, diff=diff, intent=intent, bug_details=bug_details, critique=critique, filepath=filepath)
    return pred.patch
