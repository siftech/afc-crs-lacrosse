import os
from modules.find_ftp_at_top_of_sanitizer import find_ftp_at_top_of_sanitizer
from modules.rewrite_func_given_sanitizer_delta import rewrite_func_given_sanitizer_delta
from diff import generate_repo_diff
from record import record_before_and_after, get_records_directory
from utils import read_file


def generate_patch(codebase_path, sanitizer_stderr=None, delta=None, vuln=None, harness=None):
    print("find_ftp_rewrite_func")

    ftp_relpath = find_ftp_at_top_of_sanitizer(sanitizer_stderr, codebase_path, delta)
    print("ftp_relpath=", ftp_relpath)

    ftp_abspath = os.path.join(codebase_path, ftp_relpath)
    print("ftp_abspath=", ftp_abspath)

    assert os.path.exists(ftp_abspath), f"The FTP path doesn't point to an actual file: {ftp_abspath}"

    ftp_code = read_file(ftp_abspath)

    print("ftp_code", len(ftp_code), ftp_code[:100])

    new_code = rewrite_func_given_sanitizer_delta(ftp_code, sanitizer_stderr, delta)

    print("new_code", len(new_code), new_code[:100])
    
    patch_diff = generate_repo_diff(codebase_path, ftp_relpath, new_code, base_dir=get_records_directory())

    return patch_diff
