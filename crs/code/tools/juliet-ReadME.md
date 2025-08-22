# Juliet Sanitizer (Version 1.0)

This collection of scripts and files will generate a collection of diagnostic files for a single CWE directory within the Juliet dataset.  Specifically, the sanitizer's response as well as any standard output per provided C file in the dataset.  This includes each the bad functions and the good functions in their own respective subdirectory collections.

## How to run

1. Copy (or move) the TAR file named `juliet-sanitizer-ver1.tar` into the same subdirectory as a particular `CWE###` with all its C and/or C++ files.
2. Extract the files from the tar file via `tar -xvf juliet-sanitizer-ver1.tar`.
3. Run the executable `./pipeline-juliet-sanitization`.  It will call the other scripts and use the other files as needed.
4. Reference the generated subdirectories as needed to review and analyze the information.

## Generated subdirectory list

Running `pipeline-juliet-sanitization` creates a number of subdirectories organizing the generated files.  This listing provides those directories and what they store.

- `bad_only_include_c_files`: Variants of the bad function variants per C or C++ file in the Juliet dataset format that replace non-standard include files (those specifically for Juliet dataset code) with the actual contents of the included header file.
- `good_only_include_c_files`: Variants of the good function variants per C or C++ file in the Juliet dataset format that replace non-standard include files (those specifically for Juliet dataset code) with the actual contents of the included header file.
- `include_c_files`: Variants of each C or C++ file in the Juliet dataset format that replace non-standard include files (those specifically for Juliet dataset code) with the actual contents of the included header file.  These include both the good and bad functions as well as their definition regions.
- `juliet_code_cleaned`: The bad function variants per C or C++ file in the Juliet dataset format.
- `juliet_exec_cleaned`: The good function variants per C or C++ file in the Juliet dataset format.
- `sanitized_files`: Stores the standard error from running each executable, which should be the sanitizer's results in the case of an error.  This directory further organizes the sanitizer reports by executables with the bad fuctions and by executables with the good functions.
- `stdout_files`: Stores the standard output from running each executable.  This directory further organizes the output by executables with the bad fuctions and by executables with the good functions.
- `unreliably-bad`: Stores the C or C++ code and executables for specific instances that are not consistent enough in error outcomes for analysis

## Contents of this TAR file

Although `pipeline-juliet-sanitization` will run all the scripts and files, the specific contents of the TAR file have the following purposes in case customization or future debugging becomes necessary.

- `collect-sanitizer-scripts`: Runs generated executable files (assuming they had the sanitizer flag set during compilation) and redirects their stdout and stderr for later analysis.
- `isnt-alway-bad`: A Monte-Carlo approach to assess the consistency of each program's executable with respect to errors.
- `justbad`: Generates a variant of a C or C++ code file in the Juliet dataset format that only includes the bad functions and generic code.  That is, the good functions become removed.
- `justgood`: Generates a variant of a C or C++ code file in the Juliet dataset format that only includes the good functions and generic code.  That is, the bad functions become removed.
- `Makefile`: Creates the good and bad variants of each C or C++ code file in the Juliet dataset format (see `justbad` and `justgood`) and compiles them into executable files.
- `organize-files`: Reduces clutter after running all the scripts without deleting the created files (such as `make clean`), which simply creates additional directories and stores the generated files appropriately.
- `pipeline-juliet-sanitization`: Runs the scripts in order to perform the entire procedure of generating good and bad variants of each C or C++ file in the Juliet dataset format, compiling them into executables, addressing inconsistent executables that could disrupt analysis, collects diagnostic information about their execution and sanitizer assessment, and then cleans up the content.
- `replace-includes-local`: Generates additional variants of the good and bad function variants per C or C++ file in the Juliet dataset format that replace non-standard include files (those specifically for Juliet dataset code) with the actual contents of the included header file.
- `set-aside-isnt-always-bad`: Moves inconsistent files (see `isnt-alway-bad`) elsewhere to avoid further analysis.
- `testcasesupport`: Directory from the Juliet dataset with the header and object code files that other code in the Juliet dataset might reference, which makes these essential for successful compilation.  To avoid having to edit the Makefile and guess where these files are located, this copy of those contents from the Juliet datset guarantees they are in the right place at the right time.
