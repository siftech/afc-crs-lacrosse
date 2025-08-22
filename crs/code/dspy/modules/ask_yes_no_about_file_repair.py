import dspy
import models as models
# from caching import cache_predictions
from typing import List
from modules.mixins import YesOrNoQuestionInput, FileRepairInput, CodeInput, YesOrNoAnswerOutput

class AskYesNoAboutFileRepairSignature(dspy.Signature, YesOrNoQuestionInput, FileRepairInput, CodeInput, YesOrNoAnswerOutput):
    """Answer the yes-or-no question about the provided file repair, using True for 'yes' or False for 'no'. The relevant code will be provided for reference."""

class AskYesNoAboutFileRepair(dspy.Module):
    def __init__(self, verbose=False):
        self.verbose = verbose
        self.asker = dspy.Predict(AskYesNoAboutFileRepairSignature)

    def forward(self, question, file_repair, code):
        askpred = self.asker(yes_or_no_question=question, file_repair=file_repair, code=code)
        return dspy.Prediction(answer=askpred.yes_or_no_answer)

def ask_yes_no_about_file_repair(question, file_repair, code):
    asker = AskYesNoAboutFileRepair()
    pred = asker(question, file_repair, code)
    return pred.answer
