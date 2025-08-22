import subprocess
import re
import os
import shutil
from utils import write_to_file
from advice import FAIL_APPLY_ADVICE, FAIL_BUILD_ADVICE, FAIL_POV_ADVICE, FAIL_FUNC_TESTS_ADVICE
from parse_reproduce import parse_sanitizer_stderr_with_instrumentation

CIRCA_BASEPORT = os.getenv("CIRCA_BASEPORT", None)
if CIRCA_BASEPORT is not None:
    CIRCA_BASEPORT = int(CIRCA_BASEPORT)

def can_reproduce(project_name, fuzzer_name, test_case_path):
    return project_name is not None and \
        fuzzer_name is not None and \
        test_case_path is not None

def can_build(sanitizer_name, fuzzing_engine, project_name):
    return sanitizer_name is not None and \
        fuzzing_engine is not None and \
        project_name is not None

def extract_stderr(stdout):
    return extract_sanitizer_stderr(stdout)

# FIXME: If we keep this around, it needs to be generalized to other sanitizers
def extract_sanitizer_stderr(stdout: str) -> str | None:
    # Make sure newlines are actual line breaks if needed
    stdout = stdout.encode("utf-8").decode("unicode_escape")

    # Match from ==ID==ERROR to ==ID==ABORTING
    pattern = r"(?P<full_block>==(?P<id>\d+)==ERROR:.*?==(?P=id)==ABORTING)"
    match = re.search(pattern, stdout, re.DOTALL)
    return match.group("full_block") if match else None

def apply_patch(patch, codebase_path, patch_scratch):
    apply_scratch = f"{patch_scratch}/apply_scratch"
    if os.path.exists(apply_scratch):
        shutil.rmtree(apply_scratch)
    os.makedirs(apply_scratch, exist_ok=True)
    applied_path = f"{apply_scratch}/applied_codebase"
    shutil.copytree(codebase_path, applied_path, dirs_exist_ok=True)
    patch_attempt_path = f"{apply_scratch}/patch_attempt.diff"
    write_to_file(patch_attempt_path, patch)
    apply_cmd = f"cd {applied_path}; patch -p1 < {patch_attempt_path}"
    print("apply cmd:", apply_cmd)
    apply_result = subprocess.run(apply_cmd,
                                  shell=True,
                                  capture_output=True,
                                  text=True,
                                  check=True)
    return (applied_path, patch_attempt_path)

def build(codebase_path, mod_codebase_path, sanitizer_name, fuzzing_engine, project_name):
    if CIRCA_BASEPORT is None:
        raise ValueError("CIRCA_BASEPORT is not set! Cannot run helper.py!")
    build_fuzzers_cmd = f"cd {codebase_path}/../fuzz-tooling; env -u OSS_FUZZ_SAVE_CONTAINERS_NAME DOCKER_HOST=10.0.2.2:{CIRCA_BASEPORT + 50} /lacrosse/code/prt/timestamp infra/helper.py build_fuzzers --sanitizer {sanitizer_name} --engine {fuzzing_engine} {project_name} {mod_codebase_path}"
    print("build_fuzzers cmd:", build_fuzzers_cmd)
    build_result = subprocess.run(build_fuzzers_cmd,
                                  shell=True,
                                  capture_output=True,
                                  text=True,
                                  check=True)
    return build_result

# test case and fuzzer name are oss-fuzzy ways to refer to blob and harness name
#def reproduce(codebase_path, project_name, fuzzer_name, test_case_path):
#    if CIRCA_BASEPORT is None:
#        raise ValueError("CIRCA_BASEPORT is not set! Cannot run helper.py!")
#    #reproduce_cmd = f"cd {codebase_path}/../fuzz-tooling; env -u OSS_FUZZ_SAVE_CONTAINERS_NAME DOCKER_HOST=10.0.2.2:{CIRCA_BASEPORT + 50} /lacrosse/code/prt/timestamp infra/helper.py reproduce --timeout 600 {project_name} {fuzzer_name} {test_case_path}"
#    reproduce_cmd = f"cd {codebase_path}/../fuzz-tooling; env -u OSS_FUZZ_SAVE_CONTAINERS_NAME DOCKER_HOST=10.0.2.2:{CIRCA_BASEPORT + 50} /lacrosse/code/prt/timestamp infra/helper.py reproduce {project_name} {fuzzer_name} {test_case_path}"
#    print("reproduce cmd:", reproduce_cmd)
#    repr_result = subprocess.run(reproduce_cmd,
#                                 shell=True,
#                                 capture_output=True,
#                                 text=True,
#                                 check=True)
#    return repr_result

# we've seen their CTs have inconsistent helper.py that sometimes support the --timeout flag and some do not.
# if they do not, we dont want to mistake that error for an actual reproduction.  This is tricky.

def reproduce(codebase_path, project_name, fuzzer_name, test_case_path):
    if CIRCA_BASEPORT is None:
        raise ValueError("CIRCA_BASEPORT is not set! Cannot run helper.py!")
    
    # First try with timeout
    reproduce_cmd_timeout = f"cd {codebase_path}/../fuzz-tooling; env -u OSS_FUZZ_SAVE_CONTAINERS_NAME DOCKER_HOST=10.0.2.2:{CIRCA_BASEPORT + 50} /lacrosse/code/prt/timestamp infra/helper.py reproduce --timeout 600 {project_name} {fuzzer_name} {test_case_path}"
    print("reproduce cmd (with timeout):", reproduce_cmd_timeout)
    
    try:
        repr_result = subprocess.run(reproduce_cmd_timeout,
                                     shell=True,
                                     capture_output=True,
                                     text=True,
                                     check=True)
        return repr_result
	##FIXME maybe... i'm worried if the project thinks timeout is failure, this is wrong...ideally we would
	## read the project.yaml or whatever and figure out timeouts are supposed to be failures.  But maybe
	## the repro process will just return nonzero but not throw an exception?
    except subprocess.CalledProcessError as e:
        # If it failed with timeout (code 124), don't try fallback
        if e.returncode == 124:
            print("Command timed out (exit code 124), not trying fallback")
            raise
        
        # For any other error, try without timeout
        print(f"Command with timeout failed with exit code {e.returncode}, trying without timeout")
        reproduce_cmd = f"cd {codebase_path}/../fuzz-tooling; env -u OSS_FUZZ_SAVE_CONTAINERS_NAME DOCKER_HOST=10.0.2.2:{CIRCA_BASEPORT + 50} /lacrosse/code/prt/timestamp infra/helper.py reproduce {project_name} {fuzzer_name} {test_case_path}"
        print("reproduce cmd (without timeout):", reproduce_cmd)
        
        repr_result = subprocess.run(reproduce_cmd,
                                     shell=True,
                                     capture_output=True,
                                     text=True,
                                     check=True)
        return repr_result
    
# test case and fuzzer name are oss-fuzzy ways to refer to blob and harness name
def test_w_feedback(patch, scratch_path, test_case_path, fuzzer_name, codebase_path, sanitizer_name, fuzzing_engine, project_name):
    score = 0
    
    # APPLY
    print(f"Apply the patch.")
    try:
        applied_path, patch_path = apply_patch(patch, codebase_path, scratch_path)
        score += 1
    except subprocess.CalledProcessError as e:
        feedback = f"""{FAIL_APPLY_ADVICE}
        apply failed!
        apply stdout: {e.stdout}
        apply stderr: {e.stderr}"""
        print(feedback)
        raise # FIXME: For now, raise this for debugging.  It shouldn't really happen since our pipelines are rewrite-based, so this will draw our attention to it. Remove this line later.
        return False, feedback, score
    except Exception as e:
        print("Unexpected error attempting to apply patch code:", e)
        raise

    if not can_build(sanitizer_name, fuzzing_engine, project_name):
        print(f"sanitizer_name={sanitizer_name} fuzzing_engine={fuzzing_engine} project_name={project_name}")
        print(f"Can't build. Aborting test_w_feedback early.")
        return True, None, score
    
    # BUILD
    print(f"Build the patched code.")
    try:
        build(codebase_path, applied_path, sanitizer_name, fuzzing_engine, project_name)
        score += 1
    except subprocess.CalledProcessError as e:
        feedback = f"""{FAIL_BUILD_ADVICE}
        build_fuzzers failed!
        build_fuzzers stdout: {e.stdout}
        build_fuzzers stderr: {e.stderr}"""
        return False, feedback, score
    except Exception as e:
        print("Unexpected error attempting to build patched code:", e)
        raise

    if not can_reproduce(project_name, fuzzer_name, test_case_path):
        print(f"project_name={project_name} fuzzer_name={fuzzer_name} test_case_path={test_case_path}")
        print(f"Can't test PoV. Aborting test_w_feedback early.")
        return True, None, score

    # TEST POV
    print(f"Test PoV for the built patched code.")
    try:
        repr_result = reproduce(codebase_path, project_name, fuzzer_name, test_case_path)
        score += 1
        print("reproduce returned exit code zero (no error), implying the patch must have fixed the PoV.")
        print("reproduce stdout:")
        print(repr_result.stdout)
        print("reproduce stderr:")
        print(repr_result.stderr)
    except subprocess.CalledProcessError as e:
        print("reproduce returned exit code non-zero (error), implying the patch must have failed to fix the PoV.")
        print("reproduce stdout:")
        print(e.stdout)
        print("reproduce stderr:")
        print(e.stderr)
        cleaned_stdout = parse_sanitizer_stderr_with_instrumentation(e.stdout, project_name)
        feedback = f"""{FAIL_POV_ADVICE}
        The crash was reproduced with the patched code, meaning the patch did not fix the vulnerability.
        The following stdout from the reproduce script contains the details of the crash:
        {cleaned_stdout}"""
        return False, feedback, score
    except Exception as e:
        print("Unexpected error attempting to reproduce bug:", e)
        raise

    # FUNCTIONALITY TESTS
    # FAIL_FUNC_TESTS_ADVICE

    return True, None, score
