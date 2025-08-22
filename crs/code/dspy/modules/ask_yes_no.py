import dspy
from modules.mixins import YesOrNoQuestionInput, YesOrNoAnswerOutput

class AskYesNoSignature(dspy.Signature, YesOrNoQuestionInput, YesOrNoAnswerOutput):
    """Answer the provided yes-or-no question, using True for 'yes' or False for 'no'."""

class AskYesNo(dspy.Module):
    def __init__(self, verbose=False):
        self.verbose = verbose
        self.asker = dspy.Predict(AskYesNoSignature)

    def forward(self, question):
        askpred = self.asker(yes_or_no_question=question)
        return dspy.Prediction(answer=askpred.yes_or_no_answer)

def ask_yes_no(question):
    asker = AskYesNo()
    pred = asker(question=question)
    return pred.answer
