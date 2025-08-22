import os
from modules.parse_sanitizer import parse_sanitizer_output
from modules.characterize_bug import patch_bug, diagnose_bug, critique_commit
from code_scan import read_file_lines, extract_function
from diff_scan import extract_function_changes
from utils import place_source_path
from record import record
import git_fns
from patching import diff2patch

# ATTENTION: This code is not a fully functional pipeline in the new way of doing things.
# It is here as a code reference for now.


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

def get_code_region(path, center, start_margin, end_margin, verbose=True):
    code_region = read_file_lines(path, center-start_margin, center+end_margin)
    if verbose:
        print()
        print(f"code_region({start_margin}, {end_margin}):")
        print(code_region)
    return code_region

def generate_patch(sanitizer_output, code_base_path, cp_base_path, bic_hash, bic_delta_path, vuln):
    print("bic_hash=", bic_hash)
    stack_trace, harness_call, bug_summary, bug_hint = parse_sanitizer_output(sanitizer_output)
    
    call_stack = []
    print("STACK:")
    for i, (sfunc, spath, sline, schar) in enumerate(stack_trace):
        cleanpath, relpath = place_source_path(spath, code_base_path)
        if cleanpath is None:
            continue
        print(f" --{i}--")
        print(" function=", sfunc)
        print(" path=", spath)
        print(" cleanpath=", cleanpath)
        print(" relpath=", relpath)
        print(" linenum=", sline)
        print(" charnum=", schar)
        # Subtract one because line and character numbers are one-indexed
        call_stack.append((sfunc, cleanpath, relpath, sline, schar))
        
    print()
    hfunc, hpath, hline, hchar = harness_call
    cleanhpath, relhpath = place_source_path(hpath, cp_base_path)
    print(" --HARNESS CALL--")
    print(" function=", hfunc)
    print(" path=", hpath)
    print(" cleanpath=", cleanhpath)
    print(" relpath=", relhpath)
    print(" linenum=", hline)
    print(" charnum=", hchar)

    print()
    top_fn, top_path, top_relpath, top_line, top_char = call_stack[0]
    print("top_fn=", top_fn)
    print("top_path=", top_path)
    print("top_relpath=", top_relpath)
    print("top_line=", top_line)
    print("top_char=", top_char)
    
    #codereg10 = lambda : get_code_region(top_path, top_line, 10, 10)
    print("bic_hash", bic_hash)
    
    codelines = git_fns.on_repo_at_commit(code_base_path, bic_hash, lambda repo_path: read_file_lines(top_path))
    head_codelines = read_file_lines(top_path)
    
    print("codelines len", len(codelines))
    print("head codelines len", len(head_codelines))
    
    # extract the function code
    fnsnippets, fnranges = extract_function(codelines, top_fn, top_line-1)
    numbered_headlines = ""
    for i, cline in enumerate(head_codelines):
        numbered_headlines += f"{i+1}. {cline}"
    record("numbered_headlines", numbered_headlines)
    head_fnsnippets, head_fnranges = extract_function(head_codelines, top_fn)
    
    # print(f"function definitions for {top_fn}:")
    
    if len(fnsnippets) > 1:
        raise Exception("Found multiple bug functions.")
    
    fnsnippet = fnsnippets[0]
    fnstart, fnend = fnranges[0]
    
    head_fnsnippet = head_fnsnippets[0]
    head_fnstart, head_fnend = head_fnranges[0]
    
    record("buggyfn", fnsnippet)
    
    print()
    print("bic start/end", fnstart, fnend)
    print("head start/end", head_fnstart, head_fnend)
    
    fn_diff_hunks = extract_function_changes(bic_delta_path, top_relpath, fnstart, fnend)
    bic_diff = "\n".join(fn_diff_hunks)
    # print()
    # print("BIC DELTA DIFF:")
    # print(bic_diff)
    
    record("bug_summary", bug_summary)
    record("bug_hint", bug_hint)
    
    bug_guidance = "Remember to look for vulnerabilities that result from unnecessary complexity."
    
    bug_type, bug_details = diagnose_bug(fnsnippet, fnstart, fnend, bic_diff, bug_summary, bug_hint, vuln, bug_guidance)
    record("bug_type", bug_type)
    record("bug_details", bug_details)
    
    critique_guidance = "Do not lose any of the functionality of the original commit. This means all the same non-bug-inducing inputs must be handled the same way."
    
    intent, critique = critique_commit(fnsnippet, fnstart, fnend, bic_diff, bug_details, critique_guidance)
    record("intent", intent)
    record("critique", critique)
    
    fnpatch = patch_bug(head_fnsnippet, head_fnstart, head_fnend, bic_diff, intent, bug_details, critique, top_relpath)
    record("fnpatch", fnpatch)
    
    # record("patchedfn", patchedfn)

    # fnpatch = diff2patch(fnsnippet, patchedfn)
    # record("fnpatch", fnpatch)
    
    return fnpatch
