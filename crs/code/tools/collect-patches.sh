#!/bin/bash

# Directory to search for patch files
#search_dir="/path/to/search"
search_dir=$1

# Directory to copy patch files to
#destination_dir="/path/to/destination"
destination_dir=$2

# Create the destination directory if it doesn't exist
mkdir -p "$destination_dir"

# Find all patch files and copy them to the destination directory
find "$search_dir" -type f -name "*.patch" -exec cp {} "$destination_dir" \;

echo "All patch files have been copied to $destination_dir"
