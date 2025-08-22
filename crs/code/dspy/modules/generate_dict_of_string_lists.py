import dspy
from typing import List, Dict
import models as models
from modules.extract_functions import extract_functions
from modules.mixins import GenerationInstructionsInput

class GenerateDictOfStringListsSignature(dspy.Signature, GenerationInstructionsInput):
    """Generate a dict (a.k.a. `object`, `map`) where the keys are strings and the values are lists of strings, by following the generation instructions."""
    dict_of_string_lists: Dict[str, List[str]] = dspy.OutputField(desc="The generated dict of lists, which follows the generation instructions.")

class GenerateDictOfStringLists(dspy.Module):
    def __init__(self, verbose=False):
        self.verbose = verbose
        self.generator = dspy.Predict(GenerateDictOfStringListsSignature)

    def forward(self, generation_instructions):
        print("GenerateDictOfStringLists forward")
        pred = self.generator(generation_instructions=generation_instructions)
        return dspy.Prediction(dict_of_string_lists=pred.dict_of_string_lists)

def generate_dict_of_string_lists_gpt4o(generation_instructions):
    print("generate_dict_of_string_lists_gpt4o")
    generator = GenerateDictOfStringLists()
    with dspy.context(lm = models.gpt4o_10000):
        pred = generator(generation_instructions=generation_instructions)
    return pred.dict_of_string_lists

def generate_dict_of_string_lists(generation_instructions):
    print("generate_dict_of_string_lists")
    generator = GenerateDictOfStringLists()
    pred = generator(generation_instructions=generation_instructions)
    return pred.dict_of_string_lists
