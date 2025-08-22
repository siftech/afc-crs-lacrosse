import os
import json
import dspy
from typing import List, Tuple, Union, Optional
from modules.mixins import CodeSnippetsWithLineNumbersInput, SarifInput, YesOrNoWithReasonOutput
from modules.ask_yes_no import ask_yes_no
import models
from utils import number_code_lines, current_dspy_lm
from code_scan import fix_fn_spans
from record import record
from utils import current_dspy_lm, read_file, number_code_lines

class SarifAssessmentSignature(dspy.Signature, CodeSnippetsWithLineNumbersInput, SarifInput, YesOrNoWithReasonOutput):
    """Acting as an expert cyber security researcher, determine whether the sarif description of a cyber vulnerability in the code snippets provided is internally consistent and correct with respect to the code snippets provided. (True means correct, False means incorrect.) This information may come from the code snippets provided, the sarif description, and from your background knowledge as a cyber security expert."""

class SolveSarif(dspy.Module):
  def __init__(self, verbose=False):
    self.verbose = verbose
    self.assessor = dspy.Predict(SarifAssessmentSignature)

  def record(self, name, content, printit=True):
    model_name = current_dspy_lm()
    record(f"assess_sarif_{model_name}/{name}", content, printit)

  def forward(self, codebase_path, sarif):
    # TODO
    # 1. Combine the path to the codebase with the sarif.runs.artifacts.location.uri value.
    #     codebase_path should be the path to the ctask codebase?,
    #     assuming (for example sqlite3) codebase path should suffix with: "cp_root/<task_id>/repo/"
    #     then the sarif.runs.artifacts.location.uri is "src/shell.c.in" for sqlite3-full-sarif.
    # 2. From the file extract codelines in sarif.runs.results.locations.physicalLocation.region.startLine
    #   and sarif.runs.results.locations.physicalLocation.region.endLine
    # 3. Using the extracted codelines. Supply the SAIRF, prompt the LLM for a yes or no with reason.

    print("assess_sarif_sans_pov forward method")
    self.record("codebase_path", codebase_path)
    self.record("sarif", sarif)

    # Pull data out of sarif. We expect sarif to be sarif = json.load(sarif_json_object)
    locations = [] # to store a list of tuples
    for run in sarif.get("runs", []):
      print("run is", run)
      for result in run.get("results", []):
        print("result is", result)
        for location in result.get("locations", []):
          print("location is", location)
          uri = location.get("physicallocation", {}).get("artifactlocation", {}).get("uri")
          print("uri is", uri)
          start_line = location.get("physicallocation", {}).get("region", {}).get("startline")
          print("start_line is", start_line)
          end_line = location.get("physicallocation", {}).get("region", {}).get("endline")
          print("end_line is", end_line)
          locations.append((uri, start_line, end_line))

    sarif_snippets = []
    for uri, start_line, end_line in locations:
        # Combine the uri with our codebase path... (1.)
        file_abspath = os.path.join(codebase_path, uri)
        print(f"full path to sarif uri is {file_abspath}")

        code = read_file(file_abspath)
        numbered_code = number_code_lines(code)
        numbered_codelines = numbered_code.split("\n")
        print("total code lines", len(numbered_codelines))

        # Set a margin so we grab more than just what is between startline and end_line
        # FIXME/enhancement: Could have an iterative module that expands the margin if the LLM decides it doesn't have enough context
        margin = 10
        start = max(1, start_line - margin)
        end = min(len(numbered_codelines), end_line + margin)
        
        # start-1 because they are one-indexed, and slicing(:) is inclusive of the start, exclusive of the end
        sarif_snippet_numbered_lines = numbered_codelines[start-1:end]
        sarif_snippet = "\n".join([f"From file {uri}:\n", *sarif_snippet_numbered_lines])
        sarif_snippets.append(sarif_snippet)
        
    self.record("sarif_snippets", "\n=====================\n".join(sarif_snippets), True)
    
    # Make the prediction
    asspred = self.assessor(code_snippets_with_line_numbers=sarif_snippets, sarif=sarif)
    answer, reason = asspred.yes_or_no_with_reason
    self.record("raw_answer", answer, True)
    self.record("raw_reason", reason, True)    
    if answer is True:
        answer = "correct"
    elif answer is False:
        answer = "incorrect"
    else:
        raise ValueError(f"Unexpected answer to sarif assessment: {answer}")
    self.record("answer", answer, True)
    self.record("reason", reason, True)
    return dspy.Prediction(answer=answer, reason=reason)

# @models.with_escalation([models.gpt4o_1000, models.sonnet37, models.gpt41, models.sonnet4, models.gemini25pro]) # Can go small because we're just asking it for pairs of line numbers
def assess_sarif_sans_pov(codebase_path, sarif):
  print("assess_sarif_sans_pov")
  assessor = SolveSarif()
  pred = assessor(codebase_path=codebase_path, sarif=sarif)
  return pred.answer, pred.reason
