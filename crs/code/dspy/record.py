import os
import contextlib

# Global variable to store the log directory
RECORDS_DIRECTORY = None

def record(filename, content, verbose = True, subdirectory = None):
    # Create the full file path
    #print("record", RECORDS_DIRECTORY)
    filepath = os.path.join(RECORDS_DIRECTORY, filename)

    dir_path = os.path.dirname(filepath)
    # Ensure the subdirectory exists
    os.makedirs(dir_path, exist_ok=True)

    if verbose:
        print()
        print(f"{filename}:")
        print(content)
    
    # Write the content to the file
    with open(filepath, 'w') as file:
        file.write(str(content))
        
    # print(f"Wrote to {filepath}")

def record_before_and_after(original_code, new_code):
    record("original_code", original_code, False)
    record("new_code", new_code, False)

@contextlib.contextmanager
def records_directory(directory):
    """Context manager that temporarily sets the records directory."""
    global RECORDS_DIRECTORY
    os.makedirs(directory, exist_ok=True)
    old_directory = RECORDS_DIRECTORY  # Save the current directory
    RECORDS_DIRECTORY = directory      # Set the new directory
    try:
        yield  # Allow enclosed code to execute
    finally:
        RECORDS_DIRECTORY = old_directory  # Restore the old state

def get_records_directory():
    global RECORDS_DIRECTORY
    return RECORDS_DIRECTORY
