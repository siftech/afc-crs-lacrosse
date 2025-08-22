import difflib

def diff2patch(oldfn, newfn):
    # Split strings into lines
    lines1 = oldfn.strip().splitlines()
    lines2 = newfn.strip().splitlines()

    # Generate a unified diff
    diff = difflib.unified_diff(
            lines1, lines2,
            fromfile='version1',
            tofile='version2',
            lineterm=''
        )

    # Print the diff line by line
    return "\n".join(diff)

