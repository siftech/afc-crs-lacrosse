import os
import shutil
import subprocess
from utils import place_source_path, delete_directory, replace_file_contents, ensure_trailing_newline

def generate_repo_diff(
    base_repo: str,
    relative_file_path: str,
    new_contents: str,
    scratch_dir: str = "diff_scratch",
    base_dir: str = os.getcwd(),
) -> str:
    """Creates a temporary diff environment, modifies a file, computes the diff, and cleans up."""
    print("generate_repo_diff", base_repo, relative_file_path, len(new_contents), scratch_dir, base_dir)

    new_contents = ensure_trailing_newline(new_contents)
    
    # Step 1: Create the scratch directory
    scratch_path = os.path.join(base_dir, scratch_dir)

    # Cleanup the scratch directory
    shutil.rmtree(scratch_path, ignore_errors=True)
    
    os.makedirs(scratch_path, exist_ok=True)

    # Step 2: Copy repo twice to "a" and "b" inside scratch directory
    repo_a = os.path.join(scratch_path, "a")
    repo_b = os.path.join(scratch_path, "b")

    shutil.copytree(base_repo, repo_a, dirs_exist_ok=True)
    shutil.copytree(base_repo, repo_b, dirs_exist_ok=True)
    # Step 3: Modify the file in "b"
    modified_file_path = os.path.join(repo_b, relative_file_path)
    replace_file_contents(modified_file_path, new_contents)

    # Step 4: Compute the diff from the scratch directory using --no-prefix
    result = subprocess.run(
        ["git", "diff", "--no-index", "--no-prefix", "--", "a", "b"],
        capture_output=True,
        text=True,
        cwd=scratch_path,  # Run from the scratch directory
    )

    patch = result.stdout  # Store the diff output as a string

    # FIXME: Uncomment, if we want to save space and clean up
    # Cleanup the scratch directory
    print(f"Blowing away patch-diffing scratch dir {scratch_path}")
    shutil.rmtree(scratch_path, ignore_errors=True)
    
    return patch  # Return the generated diff as a string

def generate_repo_diff_for_repairs(base_repo, repair_map, scratch_dir = "diff_scratch", base_dir = os.getcwd()):
    """Creates a temporary diff environment, modifies multiple files, computes the diff, and cleans up."""
    print("generate_repo_diff_for_repairs", base_repo, scratch_dir, base_dir)
    
    # Step 1: Create the scratch directory
    scratch_path = os.path.join(base_dir, scratch_dir)

    # Cleanup the scratch directory
    shutil.rmtree(scratch_path, ignore_errors=True)
    
    os.makedirs(scratch_path, exist_ok=True)

    # Step 2: Copy repo twice to "a" and "b" inside scratch directory
    repo_a = os.path.join(scratch_path, "a")
    repo_b = os.path.join(scratch_path, "b")

    shutil.copytree(base_repo, repo_a, dirs_exist_ok=True)
    shutil.copytree(base_repo, repo_b, dirs_exist_ok=True)

    # Step 3: Modify the relevant files in "b"
    for rel_path, new_contents in repair_map.items():
        new_contents = ensure_trailing_newline(new_contents)
        modified_file_path = os.path.join(repo_b, rel_path)
        replace_file_contents(modified_file_path, new_contents)

    # Step 4: Compute the diff from the scratch directory using --no-prefix
    result = subprocess.run(
        ["git", "diff", "--no-index", "--no-prefix", "--", "a", "b"],
        capture_output=True,
        text=True,
        cwd=scratch_path,  # Run from the scratch directory
    )

    patch = result.stdout  # Store the diff output as a string

    # FIXME: Uncomment, if we want to save space and clean up
    # Cleanup the scratch directory
    print(f"Blowing away patch-diffing scratch dir {scratch_path}")
    shutil.rmtree(scratch_path, ignore_errors=True)
    
    return patch  # Return the generated diff as a string
