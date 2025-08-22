import dspy
import os
import shutil

# This function takes a relative path which is not necessarily valid, plus a base path
# to a source code directory that is valid.  The task is to find the largest subpath
# of the relative path that, when concatenated to the base path, points to a valid
# file path.
def place_source_path(relative_path, base_path):
    # Split the relative path into parts for iterative removal
    relative_parts = relative_path.split(os.path.sep)

    # Keep trying until the relative path parts are empty
    while relative_parts:
        # Join the base path with the remaining relative parts
        potential_path = os.path.join(base_path, *relative_parts)
        rel_path = os.path.join(*relative_parts)
        # print("potential_path=", potential_path)
        if os.path.exists(potential_path):
            return potential_path, rel_path  # Return the first valid path found

        # Remove the top-level directory from the relative parts
        relative_parts.pop(0)
    
    # Return None if no valid path was found
    return None, None

def find_relative_path_embedded(relative_path, base_path):
    relative_parts = relative_path.split(os.path.sep)

    for root, dirs, files in os.walk(base_path):
        candidate_path = os.path.join(root, *relative_parts)
        if os.path.exists(candidate_path):
            # The full path to the found file/dir
            absolute_path = candidate_path
            # The relative path from the base_path (including intermediate dirs)
            relative_to_base = os.path.relpath(candidate_path, base_path)
            return absolute_path, relative_to_base

    return None, None

def get_code_region(path, center, start_margin, end_margin, verbose=True):
    code_region = read_file_lines(path, center-start_margin, center+end_margin)
    if verbose:
        print()
        print(f"code_region({start_margin}, {end_margin}):")
        print(code_region)
    return code_region

def copy_directory(original_path: str, suffix: str = "_copy") -> str:
    # Ensure the original path is a directory
    if not os.path.isdir(original_path):
        raise ValueError(f"{original_path} is not a valid directory")

    # Create the new directory name by adding the suffix
    parent_dir = os.path.dirname(original_path)
    base_name = os.path.basename(original_path)
    new_path = os.path.join(parent_dir, base_name + suffix)

    # Copy the entire directory
    shutil.copytree(original_path, new_path, dirs_exist_ok=True)
    
    return new_path  # Return the new directory path


def delete_directory(directory_path: str):
    """Deletes the specified directory and all its contents."""
    if os.path.isdir(directory_path):
        shutil.rmtree(directory_path)
        print(f"Deleted: {directory_path}")
    else:
        print(f"Directory does not exist: {directory_path}")

def replace_file_contents(file_path: str, new_contents: str):
    """Replaces the contents of a file with new_contents."""
    with open(file_path, "w", encoding="utf-8") as file:
        file.write(new_contents)
    print(f"Replaced contents of: {file_path}")

# Untested
def replace_substrings(superstring, old_substrings, new_substrings):
    if len(old_substrings) != len(new_substrings):
        raise ValueError("old_substrings and new_substrings must have the same length")
    
    for old, new in zip(old_substrings, new_substrings):
        superstring = superstring.replace(old, new)
        
    return superstring

def number_code_lines(code):
    lines = code.split("\n")
    numbered_lines = [f"{i+1}. {line}" for i, line in enumerate(lines)]
    return "\n".join(numbered_lines)

def current_dspy_lm():
    cur_lm = dspy.settings.lm
    if cur_lm:
        try:
            model_name = cur_lm.model.split("/")[1]
        except:
            model_name = cur_lm.model
    else:
        model_name = ""
    return model_name

def ensure_trailing_newline(content: str) -> str:
    """Ensure the file content ends with a newline. If missing, add one and print a warning."""
    if not content.endswith("\n"):
        print(f"Warning: Added missing newline to rewritten code! Prev last char: {repr(content[-1])}")
        return content + "\n"
    return content

# Tries to detect the correct encoding to use to read.
def read_file(filepath, err_ok=False):
    """Reads a file and returns its contents as a string, assuming UTF-8 encoding."""
    try:
        with open(filepath, "r", encoding="utf-8") as file:
            return file.read()
    except FileNotFoundError:
        print(f"Error: File '{filepath}' not found.")
    except Exception as e:
        if err_ok:
            print(f"Caught expected error reading file '{filepath}': {e}")
        else:
            print(f"Error reading file '{filepath}': {e}")
    return None

def write_to_file(filename, content):
    with open(filename, 'w') as f:
        f.write(content)

def read_codebase_file(file_relpath, codebase_path, verbose=True):
    if verbose:
        print("read_codebase_file relpath:", file_relpath)
    file_abspath = os.path.join(codebase_path, file_relpath)
    if verbose:
        print("read_codebase_file abspath=", file_abspath)
    assert os.path.exists(file_abspath), f"The repair file absolute path doesn't point to an actual file: {file_abspath}"
    return read_file(file_abspath)

def count_files_and_dirs(path):
    files_count = 0
    dirs_count = 0
    with os.scandir(path) as entries:
        for entry in entries:
            if entry.is_file():
                files_count += 1
            elif entry.is_dir():
                dirs_count += 1                
    return files_count, dirs_count

def attempt_n_times(fn, n, assert_good_answer, label=""):
    final_answer = None
    for attempt in range(1, n + 1):
        print(f"Pipeline attempt {attempt}...")
        try:
            final_answer = fn(attempt)
            assert_good_answer(final_answer, attempt)
            print(f"{label} succeeded in {attempt} attempts.")
            break  # Exit loop on success
        except Exception as e:
            if attempt == int(n):
                raise  # Re-raise the last exception
            else:
                print(f"Attempt {attempt} failed: {e}. Retrying...")
    return final_answer

def is_empty_string(s):
    if isinstance(s, str):
        if len(s) == 0:
            return True
    return False

def well_formed_sanitizer_stderr(stderr):
    # FIXME
    if stderr is None:
        return False
    if is_empty_string(stderr):
        return False
    return True

def well_formed_delta(delta):
    # FIXME
    if delta is None:
        return False
    if is_empty_string(delta):
        return False
    return True

def well_formed_blob(blob):
    # FIXME
    if blob is None:
        return False
    return True

