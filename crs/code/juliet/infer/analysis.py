#!/usr/bin/env python3
#
# Example that shows how to run an analysis with an example tool.
#
# This script uses the run_analysis method from py_common.  The run_analysis method
# script finds all the build.xml files and then passes them to the function defined
# in this file.
#
# In this case, we compile each CWE's testcases using the ant build.xml file.
#

import sys, re, os

# add parent directory to search path so we can use py_common
sys.path.append("..")

import py_common

def run_tool(build_xml_file):
	path = os.getcwd()
	results_base_dir = os.environ.get('RESULTS_DIR', None)
	if results_base_dir is None:
		print("RESULTS_DIR was not found in the environment")
		exit(1)
	suffix = re.search("(CWE\d+_.*)", path).group(1)
	results_dir = f"{results_base_dir}/{suffix}"
	command = f"infer run --results-dir {results_dir} -- ant"

	py_common.run_commands([command], True)

if __name__ == '__main__':

	# Analyze the test cases
	py_common.run_analysis("src/testcases", "build\.xml", run_tool)
