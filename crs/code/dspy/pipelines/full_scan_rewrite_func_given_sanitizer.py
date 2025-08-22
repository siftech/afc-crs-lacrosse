import os
from modules.rewrite_func_given_sanitizer import rewrite_func_given_sanitizer
from diff import generate_repo_diff
from record import record_before_and_after, get_records_directory

# This is a full-scan simulation, so we ignore the bic_sanitizer_output, bic_ftp_code, and bic_delta arguments

# In this pipeline we are just trying to use the head_ftp_code and head_sanitizer_output.

def generate_patch(head_sanitizer_output, head_ftp_code, code_base_path, ftp_path, vuln, harness, bic_sanitizer_output, bic_ftp_code, bic_delta):
    print("full_scan_rewrite_func_given_sanitizer", len(head_ftp_code), len(head_sanitizer_output), len(vuln), len(harness))

    # Write a new file
    new_code = rewrite_func_given_sanitizer(head_ftp_code, head_sanitizer_output)

    patch_diff = generate_repo_diff(code_base_path, ftp_path, new_code, base_dir=get_records_directory())

    return patch_diff

