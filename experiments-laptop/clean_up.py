import os
import shutil

# BIG BIG BIG WARNING: Only run this INSIDE the experiments-laptop directory
# Otherwise it will delete everything in the current directory

# List of filenames to keep
files_to_keep = ['code.cpp', 'code.hs', 'example.txt', 'input.txt']

# Get the current working directory
current_dir = os.getcwd()

# Traverse all subdirectories
for root, dirs, files in os.walk(current_dir):
    # Skip the current directory itself, only work on subdirectories
    if root == current_dir:
        continue
    
    # Loop through all files in the subdirectory
    for file_name in files:
        # Check if the file should be deleted
        if file_name not in files_to_keep:
            file_path = os.path.join(root, file_name)
            try:
                # Remove the file
                os.remove(file_path)
                print(f"Found to delete: {file_path}")
            except Exception as e:
                print(f"Not found to delete {file_path}: {e}")
