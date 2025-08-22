import os
from pathlib import Path
from modules.choose import choose, choose_contiguous_code_lines
import subprocess

def read_file_lines(filepath, start_line=None, end_line=None):
    """Reads lines from a file between the specified start and end line numbers."""

    try:
        with open(filepath, 'r') as file:
            lines = file.readlines()
            if end_line is not None:
                lines = lines[:end_line]
            if start_line is not None:
                # NOTE: This assumes the line numbers are one-indexed!
                lines = lines[start_line-1:]
            return lines
    except FileNotFoundError:
        raise FileNotFoundError(f"File not found: {filepath}")
    except Exception as e:
        raise RuntimeError(f"An error occurred: {e}")

def extract_function_from_code(code, fnname, refline=None):
    return extract_function(code.split("\n"), fnname, refline)
    
# This will get the definition containing the ref line, if it's not None. Otherwise it will return all of them.
def extract_function(codelines, fnname, refline=None):
    mentions = [line for line in codelines if fnname in line]
    print("mentions:")
    for line in mentions:
        print(line)
    def_line_indices = choose(mentions, f"Which of these lines of code include, or are potentially part of, the header of the definition for the function {fnname}?"
                      "Do not include the lines of code which are separate declarations. Only include definition headers. Note these lines of code are not necessarily contiguous.")
    def_mentions = [mentions[ind] for ind in def_line_indices]
    def_lines = [i for i, line in enumerate(codelines) if line in def_mentions]
    margin = 3
    start_cands = []
    for ind in def_lines:
        header_snippet = codelines[max(0, ind-margin):min(len(codelines), ind+margin)]
        print()
        print("ind=", ind)
        print("DEFN SNIPPET:")
        print(header_snippet)
        fn_start = choose_contiguous_code_lines(header_snippet, f"The first line of the function definition header for {fnname}, if it is present.",
                                                "Choose at most one line, zero if the start of the header is not contained. Note: the function header may not start on the same line containing the function name. If there are return types on the previous line, that's the start!")
        if len(fn_start) < 1:
            continue
        fn_start = fn_start[0]
        print("found fn_start", fn_start, header_snippet[fn_start])
        fn_start = ind - margin + fn_start
        print("new fn_start", fn_start, codelines[fn_start])
        start_cands.append(fn_start)

    if refline is not None:
        start_cands = [cand for cand in start_cands if cand <= refline]
        if len(start_cands) > 0:
            starts = [start_cands[-1]]
    else:
        starts = start_cands    
        
    # Now we have a start line number for the function
    ends = [extract_function_starting_with(codelines[start:], fnname)+start for start in starts]
    fn_spans = list(zip(starts, ends))
    fn_snippets = []
    for start, end in fn_spans:
        # print()
        # print("Start/end", start, end)
        fn_snippet = "".join(codelines[start:end])
        fn_snippets.append(fn_snippet)
        # print("FN snippet:")
        # print(fn_snippet)

    #convert to one-indexed line numbers before returning
    fn_spans = [(start+1, end+1) for start, end in fn_spans]
    return fn_snippets, fn_spans


def extract_function_starting_with(codelines, fnname):
    # print("extract_function_starting_with", len(codelines))
    end = None
    size = 0
    while end is None:
        size += 100
        snippet = codelines[:size]
        # print()
        # print("Snippet candidate:")
        # print("".join(snippet))
        ends = choose_contiguous_code_lines(snippet, f"Find in the code snippet the line where the function {fnname} ends. The snippet may not be long enough to contain the end, in which case return an empty list. Only select a line if you can be absolutely sure the snippet contains the end of the function, usually because you can find the beginning of the next function.",
                                            "The returned list of numbers should contain at most one number. It should be empty if the answer is ambiguous. Note that the code snippet may end before the end of the function definition!")
        if len(ends) > 0:
            end = ends[0]
    # print("Found end", end)
    return end


def get_code_region(path, center, start_margin, end_margin, verbose=True):
    code_region = read_file_lines(path, center-start_margin, center+end_margin)
    if verbose:
        print()
        print(f"code_region({start_margin}, {end_margin}):")
        print(code_region)
    return code_region

def fix_fn_spans(codelines, fn_spans):
    fixed_spans = []
    for start, end in fn_spans:
        fixed_start = start
        fixed_end = end
        try:
            preline = codelines[start-1]
            if not ((preline == "") or ("}" in preline) or ("return" in preline)):
                fixed_start = start-1
        except:
            pass
        try:
            postline = codelines[end + 1]
            if "}" in postline:
                fixed_end = end + 1
        except:
            pass
        fixed_spans.append((fixed_start, fixed_end))
    return fixed_spans

C = "C"
JAVA = "Java"

EXT_TO_LANG = {
    '.c': C,
    '.C': C,
    '.h': C,
    '.H': C,
    '.java': JAVA,
    '.JAVA': JAVA
}

def yield_source_files(root_dir, language=None):
    print("find_source_files")
    for dirpath, _, filenames in os.walk(root_dir):
        print("considering dirpath", dirpath)
        for filename in filenames:
            print("considering filename", filename)
            ext = Path(filename).suffix
            print("extension is", ext)
            if ext in EXT_TO_LANG:
                if language is None or (language == EXT_TO_LANG[ext]):
                    fp = os.path.join(dirpath, filename)
                    print("file is eligible", fp)
                    yield fp, EXT_TO_LANG[ext]

def find_source_files(root_dir, language=None):
    print("find_source_files")
    results = []
    for dirpath, _, filenames in os.walk(root_dir):
        #print("considering dirpath", dirpath)
        for filename in filenames:
            #print("considering filename", filename)
            ext = Path(filename).suffix
            #print("extension is", ext)
            if ext in EXT_TO_LANG:
                if language is None or (language == EXT_TO_LANG[ext]):
                    fp = os.path.join(dirpath, filename)
                    print(f"-{fp}")
                    results.append((fp, EXT_TO_LANG[ext]))
    print(f"Found {len(results)} eligible files.")
    return results

def filter_by_language(filepaths, language=None):
    #print("filter_by_language", language, filepaths)
    filtered = []
    for filepath in filepaths:
        #print("filepath", filepath)
        ext = Path(filepath).suffix
        #print("ext is", ext)
        if ext in EXT_TO_LANG:
            if language is None or (language == EXT_TO_LANG[ext]):
                print("passed filter_by_language", filepath)
                filtered.append(filepath)
    return filtered

def detect_language(proj_dir):
    lacrosse_path_env_var = os.environ.get("DOCKER_LACROSSE_HOME")
    detect_lang_tool = os.path.join(lacrosse_path_env_var, "code", "tools", "detect-language.sh")
    read_lang = subprocess.run([detect_lang_tool, proj_dir], stdout=subprocess.PIPE, text=True, check=True)
    return read_lang.stdout.strip()
                
