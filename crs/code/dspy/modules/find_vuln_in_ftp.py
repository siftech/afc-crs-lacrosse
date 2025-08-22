import dspy
import models as models
from utils import read_file, well_formed_delta, well_formed_blob
from modules.mixins import BuggyCodeInput, CodeFilepathInput, SanitizerStderrInput, BlobInput, BuggyCommitDeltaInput, HarnessInput, HintsInput
from custom_types import Vulnerability

# This module assumes we have a pov sanitizer stderr, a pov blob, and optionally a delta
# FIXME: We should handle the case where the PoV blob is None or malformed.
# FIXME: Either make signature(s) that accept a harness or get rid of that optional input.
# FIXME: We could reduce all these signatures down to one using the new OptionalInput mixin pattern!

# Base class.  Only code, sanitizer
class DescribeVulnerabilityInBuggyFileSignature(dspy.Signature, BuggyCodeInput, CodeFilepathInput, SanitizerStderrInput, HintsInput):
    """Acting as an expert cyber security researcher and a proficient engineer, describe the memory vulnerability in the file whose code and filepath are provided, noting that we also provide the stderr log of the memory sanitizer crash associated with the vulnerability."""
    vulnerability: Vulnerability = dspy.OutputField(desc="The vulnerability in the code.")

# code, sanitizer, delta
class DescribeVulnerabilityInBuggyFileGivenDeltaSignature(DescribeVulnerabilityInBuggyFileSignature, BuggyCommitDeltaInput):
    """Acting as an expert cyber security researcher and a proficient engineer, describe the memory vulnerability in the file whose code and filepath are provided, noting that we also provide the stderr log of the memory sanitizer crash associated with the vulnerability as well as the diff of the commit that introduced or exposed the bug."""

# code, sanitizer, blob
class DescribeVulnerabilityInBuggyFileGivenBlobSignature(DescribeVulnerabilityInBuggyFileSignature, BlobInput):
    """Acting as an expert cyber security researcher and a proficient engineer, describe the memory vulnerability in the file whose code and filepath are provided, noting that we also provide the stderr log of the memory sanitizer crash associated with the vulnerability as well as the input blob (a.k.a. proof-of-vulnerability blob) that triggered the crash."""

# code, sanitizer, delta, blob
class DescribeVulnerabilityInBuggyFileGivenBlobDeltaSignature(DescribeVulnerabilityInBuggyFileSignature, BlobInput, BuggyCommitDeltaInput):
    """Acting as an expert cyber security researcher and a proficient engineer, describe the memory vulnerability in the file whose code and filepath are provided, noting that we also provide the stderr log of the memory sanitizer crash associated with the vulnerability, the input blob (a.k.a. proof-of-vulnerability blob) that triggered the crash, and the diff of the commit that introduced or exposed the bug."""
    
class FindVulnInFtp(dspy.Module):
    def __init__(self, verbose=False):
        self.verbose = verbose
        self.vuln_finder = dspy.Predict(DescribeVulnerabilityInBuggyFileSignature)
        self.vuln_finder_delta = dspy.Predict(DescribeVulnerabilityInBuggyFileGivenDeltaSignature)
        self.vuln_finder_blob = dspy.Predict(DescribeVulnerabilityInBuggyFileGivenBlobSignature)
        self.vuln_finder_blob_delta = dspy.Predict(DescribeVulnerabilityInBuggyFileGivenBlobDeltaSignature)
        
    def forward(self, code, filepath, sanitizer_stderr, pov_blob=None, pov_harness=None, delta=None, hints="None."):
        wf_blob = well_formed_blob(pov_blob)
        wf_delta = well_formed_delta(delta)

        if wf_delta and wf_blob:
            print("find_vuln_in_ftp vuln finder uses: san, blob, delta")
            pred = self.vuln_finder_blob_delta(buggy_code=code, code_filepath=filepath, sanitizer_stderr=sanitizer_stderr, blob=pov_blob, buggy_commit_delta=delta, hints=hints)
        elif wf_delta:
            print("find_vuln_in_ftp vuln finder uses: san, delta")
            pred = self.vuln_finder_delta(buggy_code=code, code_filepath=filepath, sanitizer_stderr=sanitizer_stderr, buggy_commit_delta=delta, hints=hints)
        elif wf_blob:
            print("find_vuln_in_ftp vuln finder uses: san, blob")
            pred = self.vuln_finder_blob(buggy_code=code, code_filepath=filepath, sanitizer_stderr=sanitizer_stderr, blob=pov_blob, hints=hints)
        else:
            print("find_vuln_in_ftp vuln finder uses: san")
            pred = self.vuln_finder(buggy_code=code, code_filepath=filepath, sanitizer_stderr=sanitizer_stderr, hints=hints)

        vuln = pred.vulnerability
        return dspy.Prediction(vuln=vuln)

def find_vuln_in_ftp(code, filepath, sanitizer_stderr, pov_blob=None, pov_harness=None, delta=None, hints="None."):
    print("find_vuln_in_ftp")
    finder = FindVulnInFtp()

    pred = finder(code=code, filepath=filepath, sanitizer_stderr=sanitizer_stderr, pov_blob=pov_blob, delta=delta, pov_harness=pov_harness, hints=hints)
    vuln = pred.vuln

    return vuln
