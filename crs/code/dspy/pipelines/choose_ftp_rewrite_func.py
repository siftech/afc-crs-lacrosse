import os
from modules.find_ftp_at_top_of_sanitizer import find_ftp_at_top_of_sanitizer
from modules.rewrite_func_given_sanitizer_delta import rewrite_func_given_sanitizer_delta
from modules.rewrite_func_given_sanitizer_blob_delta import rewrite_func_given_sanitizer_blob_delta
#from modules.rewrite_func_given_sanitizer_delta_blob_harness import rewrite_func_given_sanitizer_delta_blob_harness
from diff import generate_repo_diff
from record import record_before_and_after, get_records_directory
from utils import read_file, is_well_formed_sanitizer_stderr, is_well_formed_delta

# Note: All pipelines will assume a non-null codebase_path, but this one additionally
# assumes a non-null sanitizer_stderr, at least for now.  Assuming it stays this way,
# we will want to dispatch to this only in cases where we have a sanitizer_stderr.

def generate_patch(codebase_path, sanitizer_stderr=None, delta=None, pov_blob=None, pov_harness=None):
    print("choose_ftp_rewrite_func: generate_patch", type(codebase_path), type(sanitizer_stderr), type(delta), type(pov_blob), type(pov_harness))

    # First choose a single FTP
    if well_formed_sanitizer_stderr(sanitizer_stderr):
        ftp_relpath = find_ftp_at_top_of_sanitizer(sanitizer_stderr, codebase_path, delta)

        if well_formed_delta(delta):
        # Here we have to choose an FTP based on delta and code

    else:
    print("ftp_relpath=", ftp_relpath)

    ftp_abspath = os.path.join(codebase_path, ftp_relpath)
    print("ftp_abspath=", ftp_abspath)

    assert os.path.exists(ftp_abspath), f"The FTP path doesn't point to an actual file: {ftp_abspath}"

    ftp_code = read_file(ftp_abspath)

    # Next, repair the FTP by selecting and rewriting individual functions.
    if pov_blob is None:
        new_code = rewrite_func_given_sanitizer_delta(ftp_code, sanitizer_stderr, delta)
    elif pov_harness is None:
        new_code = rewrite_func_given_sanitizer_blob_delta(ftp_code, sanitizer_stderr, delta, pov_blob)
    #else:
    #    new_code = rewrite_func_given_sanitizer_blob_delta_harness(ftp_code, sanitizer_stderr, delta, pov_blob, pov_harness)

    # Finally, generate a unified diff using the repaired FTP
    patch_diff = generate_repo_diff(codebase_path, ftp_relpath, new_code, base_dir=get_records_directory())

    print("patch length", len(patch_diff))
    
    assert len(patch_diff) > 0, "The patch is empty"
    
    return patch_diff
