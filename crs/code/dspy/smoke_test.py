import os
import sys

dspy_path = os.path.dirname(os.path.abspath(__file__))
print("dspy path is", dspy_path)
if dspy_path not in sys.path:
    sys.path.append(dspy_path)

from modules.map_strs import map_strs_gpt4o

countries = ["USA", "Russia", "Brazil"]
print(f"countries are {countries}")

inst = "Name the capital of each country"

capitals = map_strs_gpt4o(countries, inst)
print(f"capitals are {capitals}")



