import re

def extract_function_changes(delta, file_path, start_line, end_line):
    lines = delta.split("\n")

    # Regex to identify file diffs
    file_diff_pattern = rf"^diff --git a/{re.escape(file_path)} b/{re.escape(file_path)}"
    # Regex to identify hunk headers (e.g., @@ -123,7 +123,8 @@)
    hunk_header_pattern = r"^@@ -\d+,\d+ \+(\d+),(\d+) @@"

    in_target_file = False
    changes = []
    for i, line in enumerate(lines):
        # Check if we're in the correct file
        if re.match(file_diff_pattern, line):
            in_target_file = True
        elif line.startswith("diff --git") and in_target_file:
            # Reached the next file diff, stop if we were in the target file
            in_target_file = False

        if in_target_file and re.match(hunk_header_pattern, line):
            # Parse hunk header
            match = re.match(hunk_header_pattern, line)
            new_line = int(match.group(1))
            new_count = int(match.group(2))
            
            # Calculate the range of lines affected in the new version
            new_range_start = new_line
            new_range_end = new_line + new_count - 1

            # print("TARGET FILE HUNK:", new_line, new_count, new_range_start, new_range_end)
            # print("start_line=", start_line, "end_line=", end_line)
            
            # Check if the hunk overlaps with the function's line range
            if new_range_start <= end_line and new_range_end >= start_line:
                # print("Hunk in range!")
                # Collect the entire hunk (header + associated lines)
                hunk_lines = [line]
                j = i + 1
                while j < len(lines) and not lines[j].startswith("diff --git") and not re.match(hunk_header_pattern, lines[j]):
                    hunk_lines.append(lines[j])
                    j += 1
                changes.append("".join(hunk_lines))
                
    return changes
