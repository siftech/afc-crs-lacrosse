import subprocess
from modules.extract_sanitizer_stderr import extract_sanitizer_stderr_with_instrumentation

def strip_ansi(ansi_string):
    result = subprocess.run(
        ['ansi2txt'],
        input=ansi_string,
        text=True,
        capture_output=True,
        check=True
    )
    return result.stdout

def clean_fuzzing_output(content):
    content = strip_ansi(content).replace("OSS_FUZZ_", "")
    return content.encode("utf-8").decode("unicode_escape")

def parse_sanitizer_stderr_with_instrumentation(sanitizer_stderr, project_name):
    instrumentation_snippets = None # only applicable to java/jazzer, IIUC.
    # Ensure the sanitizer stderr is cleaned
    sanitizer_stderr = clean_fuzzing_output(sanitizer_stderr)
    if sanitizer_stderr is not None:
        # Parse the stacktrace out of the reproduce
        print("Parsing out stack trace plus any relevant instrumentation...")
        extracted_sanitizer_stderr = extract_sanitizer_stderr_with_instrumentation(sanitizer_stderr, project_name=project_name)
        if extracted_sanitizer_stderr is not None and len(extracted_sanitizer_stderr) > 0:
            sanitizer_stderr = extracted_sanitizer_stderr
        else:
            print("Failed to parse a stack trace out of the reproduce path contents. sanitizer_stderr is", sanitizer_stderr)
        return sanitizer_stderr

