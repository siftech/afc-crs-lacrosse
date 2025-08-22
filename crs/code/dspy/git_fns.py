import subprocess
import os

def run_git_command(repo_path, command):
    """Run a Git command in the specified repository."""
    result = subprocess.run(
        ["git"] + command,
        cwd=repo_path,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE
    )
    if result.returncode != 0:
        print("Running git command:")
        print(result.args)
        raise RuntimeError(f"Git command failed: {result.stderr}")
    return result.stdout.strip()

def checkout_commit(repo_path, commit_hash):
    """Checkout a specific commit."""
    print(f"Checking out commit {commit_hash}...")
    run_git_command(repo_path, ["checkout", commit_hash])

def checkout_head(repo_path):
    """Checkout the HEAD of the current branch."""
    print("Checking out HEAD...")
    run_git_command(repo_path, ["checkout", "HEAD"])

def on_repo_at_commit_old(repo_path, commit_hash, process_repo):
    """
    Perform operations on the repo at a specific commit then revert to HEAD.
    """
    print("commit_hash", commit_hash)
    # Save the current branch/commit state
    current_commit = run_git_command(repo_path, ["rev-parse", "HEAD"])
    print(f"Current commit: {current_commit}")
    try:
        # Checkout the specific commit
        checkout_commit(repo_path, commit_hash)
        # Perform the required operation at this commit
        return process_repo(repo_path)
    finally:
        # Restore the repo to HEAD
        checkout_head(repo_path)

def on_repo_at_commit(repo_path, commit_hash, process_repo):
    """
    Perform operations on the repo at a specific commit then revert to the previous branch/commit.
    """
    # Save the current branch/commit state
    current_ref = run_git_command(repo_path, ["rev-parse", "--abbrev-ref", "HEAD"])
    if current_ref == "HEAD":  # If already detached, save the commit hash instead
        current_ref = run_git_command(repo_path, ["rev-parse", "HEAD"])
    print(f"Current branch/commit: {current_ref}")
    try:
        # Checkout the specific commit
        checkout_commit(repo_path, commit_hash)
        # Perform the required operation at this commit
        return process_repo(repo_path)
    finally:
        # Restore the repo to the previous branch/commit
        print(f"Restoring repo to {current_ref}...")
        run_git_command(repo_path, ["checkout", current_ref])
