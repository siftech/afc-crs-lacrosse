import dspy
import models as models
from modules.extract_functions import extract_functions
from modules.generate_dict_of_string_lists import generate_dict_of_string_lists_gpt4o

class FindDeltaFilesAndFunctions(dspy.Module):
    def __init__(self, verbose=False):
        self.verbose = verbose

    def forward(self, delta):
        # Get the functions/files that show up in the diff
        fn_map = generate_dict_of_string_lists_gpt4o(f"""The following is the full contents of a unified diff representing git commit. Construct a dict that maps every mentioned filepath to the names of its modified functions, excluding files that are not code files. The keys should be strings representing the complete set of relative filepaths (for code files) as they appear in the diff, and each value should be a list of strings representing the complete list of names of mentioned/modified functions names from that filepath.

Unified diff contents:
{delta}""")

        return dspy.Prediction(fn_map=fn_map)

def find_delta_files_and_functions(delta):
    print("find_delta_files_and_functions")
    finder = FindDeltaFilesAndFunctions()
    pred = finder(delta=delta)
    return pred.fn_map
