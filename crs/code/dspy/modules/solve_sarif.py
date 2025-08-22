import dspy
from typing import List, Tuple, Union, Optional
from modules.mixins import NumberedLogLinesInput, SarifInput, YesOrNoWithReasonOutput
from modules.ask_yes_no import ask_yes_no
import models
from utils import number_code_lines, current_dspy_lm
from code_scan import fix_fn_spans
from custom_types import Vulnerability
from record import record
import json

class SarifPOVAssessmentSignature(dspy.Signature, NumberedLogLinesInput, SarifInput, YesOrNoWithReasonOutput):
    # TODO
    pass

class SolveSarifPOV(dspy.Module):
  def __init__(self, verbose=False):
    self.verbose = verbose
    self.assessor = dspy.Predict(SarifPOVAssessmentSignature)

  def record(self, name, content):
    model_name = current_dspy_lm()
    record(f"assess_sarif_{model_name}/{name}", content, True)

  def forward():
    # TODO
    # Pull target file path and target functions out of vulnerablity
    # Pull code out of target filepath,
    #       for i, vuln in enumerate(vulns):
    #         self.record(f"vuln_{i}", str(vuln))
    #         file_repairs = vuln.suggested_file_repairs
    #         vuln_desc = vuln.description

    #         for file_repair in file_repairs:
    #             file_relpath = file_repair.target_filepath
    #             print("repair file relpath:", file_relpath)
    #             file_abspath = os.path.join(codebase_path, file_relpath)
    #             print("repair file abspath=", file_abspath)
    #             assert os.path.exists(file_abspath), f"The repair file absolute path doesn't point to an actual file: {file_abspath}"
    #             original_code = read_file(file_abspath)
    #             self.record("original_code", original_code)
    #
    # Extract functions mentioned by vuln with their line numbers

    pass

# @models.with_escalation([models.gpt4o_1000, models.sonnet37, models.gpt41, models.sonnet4, models.gemini25pro]) # Can go small because we're just asking it for pairs of line numbers
def assess_sarif(vuln: Vulnerability, codebase_path, sarif):
  # TODO

  pass