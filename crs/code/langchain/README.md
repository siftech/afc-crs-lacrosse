# Langchain Experiments

This is a temporary repository, for the Lacrosse/AIxCC project, that collects experiments, primarily as Jupyter Notebooks, about using the Python (and JavaScript) `langchain` library to automate LLM queries for Lacrosse.

My expectation is that this will provide a curated list of techniques that we can transfer into the Lacrosse project repository when they have matured, without messing up the Lacrosse code repo.  If desired, a stable state of this repo could also be imported into the Lacrosse repo proper.

# How to install

The notebooks in the repo expect to be executed in a `poetry` Python virtual environment.  I am by no means an expert on `poetry`! I believe the following instructions should serve:

1. Install `poetry` in some way that's appropriate for your platform.
2. `cd` to the directory and do `poetry install`
3. For repeated interactions in the environment, do `poetry shell`, or for a specific command, use `poetry run`

# To Explore the Notebooks

After installation, `cd` to the `notebooks/` subdirectory, and do `poetry run jupyter notebook` (or `poetry run jupyter lab`).

# Using OpenAI Models

Using any of OpenAI's models through LangChain requires an OpenAI API key.
To acquire one, you will need to have an account with OpenAI.
You can set one up through their [website](https://platform.openai.com/) by clicking `Sign up` at the top-right corner.
Once you have created an account and/or logged in, you can acquire an OpenAI API key by clicking the lock in the left-hand panel (alternatively by going directly to the page [here](https://platform.openai.com/api-keys)).

Once you have an API key, you will need to allocate credits to use their API calls.
On their website, go to `Settings -> Billing` and click on `Add to credit balance`.
Here, you can allocate an amount of credits (in dollars); Pavan has usually done $5 when running small tests.
If you want to (and trust yourself), you can set up `Auto recharge`, but that's up to you.
Once you have allocated credits, you can use OpenAI models through LangChain.

# Juliet Utilities

This repo also contains a few Python utilities that are relevant to the Juliet test cases, such as separating the test cases by their good and bad functions and obfuscating the functions.

## Setting up the Obfuscator

The obfuscator we use is a variation on [terminaldweller's obfuscator](https://terminaldweller.github.io/mutator/obfuscator/).
The obfuscator has the ability to replace names of variables, functions, and files; however, we currently obfuscate the names of functions and files as variable replacement seems to not function properly from inital testing.

Before you use the obfuscator, you will first need to to decompress and compile the obfuscator:
```
cd obfuscator-code
tar -xzvf obfuscator-code/terminaldweller-mutator-sift-version.tar.gz
cd terminaldweller-mutator-sift-version/obfuscator/
make -j8
```
This will create an executable `obfuscator` in `obfuscator-code/terminaldweller-mutator-sift-version/obfuscator/` and will be used by the utilities to obfuscate Juliet test case functions.

> **NOTE**: When compiling the obfuscator, Pavan ran into an issue with llvm and was able to fix it by changing the `LLVM_CONF` variable in `obfuscator-code/terminaldweller-mutator-sift-version/obfuscator/Makefile` from `llvm-config` to `llvm-config-10`.
The obfuscator code that was decompressed contains this change.

## Using the Juliet Utilities

The Python Juliet utilites are provided in `lacrosse_llm/juliet_utils.py` and contain the following key functions:
1. `get_function` - Gets a good or bad function from the code of a provided test case. Good or bad function is defined by the patterns `#ifndef OMITGOOD` (good functions) or `#ifndef OMITBAD` (bad functions); one of these has to be passed into `get_function`.
    - Example: `get_function(lines, '#ifndef OMITGOOD')` and `get_function(lines, '#ifndef OMITBAD')` where `lines` is a list of the lines in a test case file.
2. `write_function` - Write a function to a file.
3. `obfuscate_code` - Obfuscates code in a given file using terminaldweller's obfuscator. This function takes in a file containing code and the path of the obfuscator, and returns a filename containing the obfuscated code. This function also takes in two additional parameters, `shake` and `shake_length`. These are used to customize the length of the hash strings that replace function and file names and does not need to be adjusted unless needed.
4. `process_test_case` - Wraps `get_function` and `write_function` functionality. `process_test_case` takes a Juliet test case file and provides a dictionary containing the good and bad functions, and names of files containing them.

Below is a sample workflow of the utilities:
```
JULIET_TEST_FILE = "juliet-file.cpp"
OBFUSCATOR_PATH = "obfuscator-code/terminaldweller-mutator-sift-version/obfuscator/obfuscator"
response = process_test_case(JULIET_TEST_FILE)
good_function_file_obfuscated = obfuscate_code(response['good-function-filename'], OBFUSCATOR_PATH)
bad_function_file_obfuscated = obfuscate_code(response['bad-function-filename'], OBFUSCATOR_PATH)
```

An example of this workflow connected to an LLM can be found in `lacrosse_llm/confirm_vuln_juliet_testcase.py`.
