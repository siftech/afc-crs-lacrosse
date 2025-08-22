"""
This file contains functions to ensure that the filenames in a
patch header are appropriate for the AIxCC competition.
"""

import os
import re
from typing import Tuple

from lacrosse_llm.utils import find_git_root


def repair_patch_header(filename: str, patch_str: str) -> str:
    """
    `filename` should be the filename to insert as the file to patch,
    `patch_str` is the text of the patch to be modified.  Returns the
    modified patch.
    """
    new_patch: str
    return new_patch


def find_modified_file(patch: str) -> re.Match:
    m = re.search(r'^\+\+\+\W+(b/)?(\S+\.\w+)$', patch, re.MULTILINE)
    if m is None:
        raise ValueError(f"Unable to find modified file ({patch}) in patch file header.")
    return m


def subst_pattern(match: re.Match, new_modified_filename: str) -> Tuple[str, str]:
    """"
    Return a pair of lines: one the line in the patch to be replaced, and
    one the line in the patch to replace it with.
    """
    old_line = match.group(0)
    new_line = old_line.replace(match.group(2), new_modified_filename, 1)
    return old_line.rstrip("\r\n "), new_line.rstrip("\r\n ")


def repair_patch(patch: str, rel_path: str) -> str:
    m: re.Match = find_modified_file(patch)
    old, new = subst_pattern(m, rel_path)
    new_patch = patch.replace(old, new, 1)
    return new_patch


def generate_repaired_patch(patch_filename: str, patched_filename: str) -> str:
    """
    Generate a repaired patch file for `patch_filename` in which we substitute
    the proper destination pathname for `patched_filename`.
    """
    patch_file = os.path.abspath(patch_filename)
    patched_file = os.path.abspath(patched_filename)
    git_root = find_git_root(patched_file)
    rel_path = os.path.relpath(patched_file, git_root)
    patch: str
    with open(patch_file, 'r') as f:
        patch = f.read()
    new_patch = repair_patch(patch, rel_path)
    return new_patch
