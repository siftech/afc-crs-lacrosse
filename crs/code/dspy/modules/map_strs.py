import dspy
import models as models
from typing import List

class MapStrsSignature(dspy.Signature):
    """Transform the list of input strings into a list (of the same length) of output strings, by following the transform instructions."""
    transform_instructions: str = dspy.InputField(desc="Instructions for how to transform the inputs into outputs.")
    inputs: List[str] = dspy.InputField(desc="The inputs.")
    outputs: List[str] = dspy.OutputField(desc="The outputs. This list should be the same length as the inputs.")

class MapStrs(dspy.Module):
    def __init__(self, verbose=False):
        self.verbose = verbose
        self.mapper = dspy.Predict(MapStrsSignature)

    def forward(self, inputs, instructions):
        mappred = self.mapper(transform_instructions=instructions, inputs=inputs)
        return dspy.Prediction(outputs=mappred.outputs)

# This one is self-contained with the llm wrapped. You can call it from any old python code.
def map_strs_gpt4o(inputs, instructions):
    mapper = MapStrs()
    with dspy.context(lm = models.gpt4o_10000):
        mappred = mapper(inputs=inputs, instructions=instructions)
    return mappred.outputs

# You can use this one if you're calling from code that is wrapped somewhere in
# with dspy.context(lm = model):
def map_strs(inputs, instructions):
    mapper = MapStrs()
    mappred = mapper(inputs=inputs, instructions=instructions)
    return mappred.outputs
                                    
