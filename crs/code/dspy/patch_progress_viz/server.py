#!/usr/bin/env python3

import argparse
from flask import Flask, jsonify, render_template_string
import pandas as pd
import altair as alt
import json
import os
import re
import glob

app = Flask(__name__)

results_dirs = None

# Path to the data file (assumed to be JSON, can be changed)
SAMPLE_DATA_FILE = "latest_data.json"
# PATCH_RESULTS_DIR = "$LACROSSE_HOME/code/test/"

# Define your custom order
APPROACH_ORDER1 = [
    "code-delta--discover_then_patch_vulns--sonnet37-gpt4o",
    #"code-delta--discover_then_patch_vulns--gpt4o",
    #"code-delta--discover_then_patch_vulns--sonnet37",
    "code-delta-sani--discover_then_patch_vulns--sonnet37-gpt4o",
    #"code-delta-sani--discover_then_patch_vulns--gpt4o",
    #"code-delta-sani--discover_then_patch_vulns--sonnet37",
    "code-delta-sani-blob--discover_then_patch_vulns--sonnet37-gpt4o",
    #"code-delta-sani-blob--discover_then_patch_vulns--gpt4o",
    #"code-delta-sani-blob--discover_then_patch_vulns--sonnet37",
]  

APPROACH_ORDER2 = [
    "code-delta--discover_then_patch_vulns_w_feedback--sonnet37-gpt4o",
    #"code-delta--discover_then_patch_vulns_w_feedback--gpt4o",
    #"code-delta--discover_then_patch_vulns_w_feedback--sonnet37",
    "code-delta-sani--discover_then_patch_vulns_w_feedback--sonnet37-gpt4o",
    #"code-delta-sani--discover_then_patch_vulns_w_feedback--gpt4o",
    #"code-delta-sani--discover_then_patch_vulns_w_feedback--sonnet37",
    "code-delta-sani-blob--discover_then_patch_vulns_w_feedback--sonnet37-gpt4o",
    #"code-delta-sani-blob--discover_then_patch_vulns_w_feedback--gpt4o",
    #"code-delta-sani-blob--discover_then_patch_vulns_w_feedback--sonnet37"
]  

def immediate_subdirs(mydir):
    with os.scandir(mydir) as it:
        dirs = []
        for entry in it:
            if entry.is_dir():
                dirs.append(entry.path)
        return dirs

def dir_files(mydir):
    with os.scandir(mydir) as it:
        files = []
        for entry in it:
            if entry.is_file():
                files.append(entry.name)
        return files
        
def list_results_files_with_name(name):
    #print("list_results_files_with_name", name)
    matched_files = []
    #regex = re.compile(pattern)
    #print(f"Compiled pattern: {regex.pattern}")

    for results_dir in results_dirs:
        #print("results_dir=", results_dir)
        for subdir in immediate_subdirs(results_dir):
            #print("subdir=", subdir)
            testdir = os.path.join(subdir, "test-patch")
            for filename in dir_files(testdir):
                #print("filename=", filename)
                if filename == name:
                    full_path = os.path.join(testdir, filename)
                    #print("matching file", full_path)
                    matched_files.append(full_path)
            rel_path = str(os.path.relpath(testdir, results_dir))

    return matched_files


# Function to parse log files and extract problem scores
def gather_results():
    results = []
    
    # Regex patterns to extract relevant data
    problem_pattern = re.compile(r"PROBLEM=(cpv\d+)")
    stage_patterns = {
        "apply": re.compile(r"\*PASS\* apply"),
        "build": re.compile(r"\*PASS\* build"),
        "run_pov": re.compile(r"\*PASS\* run_pov"),
        "run_tests": re.compile(r"\*PASS\* run_tests")
    }
    
    results_files = list_results_files_with_name("test.log")

    #print("results files:")
    for results_file in results_files:
        #print(results_file)
        
        path_parts = results_file.split(os.sep)
        #print(path_parts)

        problem = path_parts[-4]
        approach = path_parts[-3]
        
        # Read and parse log file
        with open(results_file, "r") as f:
            stages_passed = 0
            
            for line in f:
                # Count passed stages
                for stage, pattern in stage_patterns.items():
                    if pattern.search(line):
                        stages_passed += 1
                        
            results.append({"approach": approach, "problem": problem, "score": stages_passed})
            
    return results
        
# Function to load data dynamically
def load_data():
    try:
        # with open(SAMPLE_DATA_FILE, "r") as f:
        #     raw_data = json.load(f)
        raw_data = gather_results()

        expanded_data = []

        # prev_approach = None  # Track last seen approach
        approach_offsets = {}
        
        for row in raw_data:
            approach, problem, score = row["approach"], row["problem"], row["score"]

            if approach in approach_offsets.keys():
                start = approach_offsets[approach]
            else:
                start = 0
            
            end = start + score

            # Assign stacked units
            for unit in range(start, end):
                expanded_data.append({
                    "Approach": approach,
                    "Problem": problem,
                    "Unit": unit
                })
            
            # Update the offset for the next problem in the same approach
            approach_offsets[approach] = end
            
        return pd.DataFrame(expanded_data)
                    
    except Exception as e:
        print(f"Error loading data: {e}")
        return pd.DataFrame(columns=["Approach", "Problem", "Unit"])  # Return empty DataFrame if error
        
    
# Function to generate Altair chart
def generate_chart():

    df = load_data()

    max_y = 4 * df["Problem"].nunique()  # Calculate 4 × number of unique problems
    df = df.sort_values(by=["Problem"], ascending=True)
    
    # Get unique values in 'Approach' and determine unknown values
    unique_approaches = set(df["Approach"].dropna().unique())  # Convert to set
    known_approaches = set(APPROACH_ORDER1).union(set(APPROACH_ORDER2))
    unknown_approaches = sorted(unique_approaches - known_approaches)  # Sort unknowns alphabetically

    #print()

    seps1 = [{
        "Approach": "S1",
        "Problem": "separator",
        "Unit": i
    } for i in range(0, max_y)]

    seps2 = [{
        "Approach": "S2",
        "Problem": "separator",
        "Unit": i
    } for i in range(0, max_y)]
    
    df = pd.concat([df, pd.DataFrame(seps1), pd.DataFrame(seps2)], ignore_index=True)
    
    # Create a categorical order combining known and unknown approaches
    ordered_categories = APPROACH_ORDER1 + ["S1"] + APPROACH_ORDER2 + ["S2"] + unknown_approaches

    # ordered_categories.insert(len(APPROACH_ORDER), separator_label)

    df["Approach"] = pd.Categorical(df["Approach"], categories=ordered_categories, ordered=True)
                            
    df = df.sort_values(by=["Approach"])  # Sort by the prescribed order

    df["ColorCategory"] = df["Problem"]
    df.loc[df["Problem"] == "separator", "ColorCategory"] = None  # So it won’t show in legend
    
    # df["ColorValue"] = df["Problem"]
    # df.loc[df["Problem"] == "separator", "ColorValue"] = "SEPARATOR_MARKER"
    
    # print(json.dumps(APPROACH_ORDER, indent=4))
    
    # Print the DataFrame for debugging
    #print("\n---- DEBUG: DataFrame Before Chart Generation ----")
    #print(df.to_string(index=False))  # Print entire DataFrame without truncation
    #print("--------------------------------------------------\n")                
    
    if df.empty:
        return None
    
    df["UnitStart"] = df["Unit"]   # Lower bound of the unit
    df["UnitEnd"] = df["Unit"] + 1    # Upper bound of the unit

    regular_rows = df[df["Problem"] != "separator"]
    separator_rows = df[df["Problem"] == "separator"]

    # Layer 1: Regular data with full color encoding
    main_chart = alt.Chart(regular_rows).mark_bar(
        stroke='black',
        strokeWidth=0.5
    ).encode(
        x=alt.X("Approach:N", title="Approach", axis=alt.Axis(labelLimit=1000, labelAngle=45), sort=ordered_categories),
        y=alt.Y("UnitStart:Q", title="Total Score", scale=alt.Scale(domain=[0, max_y])),
        y2=alt.Y2("UnitEnd:Q"),
        color=alt.Color("Problem:N", title="Problem", sort=alt.SortField("Problem", order="descending")),
        order=alt.Order("Problem:Q", sort="descending")
    )

    # Layer 2: Separator bar, fixed color, no legend
    separator_chart = alt.Chart(separator_rows).mark_bar(
        stroke='black',
        strokeWidth=0.5,
        color='black'  # Fixed color
    ).encode(
        x=alt.X("Approach:N", sort=ordered_categories),
        y=alt.Y("UnitStart:Q", title="Total Score", scale=alt.Scale(domain=[0, max_y])),
        y2="UnitEnd:Q"
    )

    # Combine them
    chart = (main_chart + separator_chart).properties(height=400)
    
    chart_json = chart.to_json()  # Return Vega-Lite JSON
    # print("chart_json=")
    # print(chart_json)
    return chart_json


# Route to serve the chart JSON
@app.route("/chart-data")
def chart_data():
    chart_json = generate_chart()
    if chart_json:
        return jsonify(json.loads(chart_json))
    else:
        return jsonify({"error": "No data available"}), 500
    
# HTML page to display the visualization
HTML_TEMPLATE = """
<!DOCTYPE html>
<html>
<head>
<title>Live Data Visualization</title>
<script src="https://cdn.jsdelivr.net/npm/vega@5"></script>
<script src="https://cdn.jsdelivr.net/npm/vega-lite@5"></script>
<script src="https://cdn.jsdelivr.net/npm/vega-embed@6"></script>
</head>
<body>
<h1>Patching Pipelines</h1>
<div id="vis"></div>
<script>
function updateChart() {
fetch('/chart-data')
.then(response => response.json())
.then(spec => vegaEmbed('#vis', spec))
.catch(console.error);
}
updateChart();  // Load on first visit
setInterval(updateChart, 5000);  // Auto-refresh every 5 seconds
</script>
</body>
</html>
"""

# Route to serve the visualization page
@app.route("/")
def index():
    return render_template_string(HTML_TEMPLATE)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Start the Flask server with glob of results dirs.")
    parser.add_argument(
        '--results',
        type=str,
        nargs='+',  # Accept one or more space-separated arguments
        help="List of results dir paths"
    )
    args = parser.parse_args()
    #print("args.results=", args.results)
    results_dirs = args.results if args.results else glob.glob("results/cpv*")
    
    base_port_env = os.getenv("CIRCA_BASEPORT")

    if base_port_env is None:
        raise ValueError("CIRCA_BASEPORT environment variable must be set.")

    base_port = int(base_port_env)
    port = base_port + 888

    print()
    print(f"Patch results now displayed at {port}. Use incognito mode. Note 'http' not 'https'.")

    app.run(host="0.0.0.0", port=port, debug=True)
