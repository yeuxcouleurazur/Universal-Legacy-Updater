import os
import shutil
import subprocess
import sys
from pathlib import Path

def remove_temporary_files(path):
    """Remove temporary files from the project directory."""
    temp_patterns = ['*.tmp', '*.bak', '*.swp', '*.pyc', '__pycache__', '*.log', '.DS_Store']
    removed = []
    for root, dirs, files in os.walk(path):
        for pattern in temp_patterns:
            if pattern.startswith('*.'):
                for file in files:
                    if file.endswith(pattern[1:]):
                        filepath = os.path.join(root, file)
                        os.remove(filepath)
                        removed.append(filepath)
            elif pattern in dirs:
                dirpath = os.path.join(root, pattern)
                shutil.rmtree(dirpath)
                removed.append(dirpath)
    return removed

def normalize_code_style(path):
    """Normalize code style using black if available."""
    try:
        # Check if black is installed
        subprocess.run([sys.executable, '-m', 'black', '--check', '--diff', path], check=True, capture_output=True)
        # If no error, run black
        subprocess.run([sys.executable, '-m', 'black', path], check=True)
        return "Code style normalized with black."
    except subprocess.CalledProcessError:
        return "Black not available or failed to run."
    except FileNotFoundError:
        return "Black not installed."

def organize_project_structure(path):
    """Organize project structure by creating standard directories and moving files."""
    # This is a basic implementation; can be expanded
    src_dir = os.path.join(path, 'src')
    tests_dir = os.path.join(path, 'tests')
    docs_dir = os.path.join(path, 'docs')

    os.makedirs(src_dir, exist_ok=True)
    os.makedirs(tests_dir, exist_ok=True)
    os.makedirs(docs_dir, exist_ok=True)

    # Move Python files to src if not already there
    for root, dirs, files in os.walk(path):
        if root == path:  # Only top level
            for file in files:
                if file.endswith('.py') and not file.startswith('test_'):
                    src_file = os.path.join(root, file)
                    dest_file = os.path.join(src_dir, file)
                    if not os.path.exists(dest_file):
                        shutil.move(src_file, dest_file)
                elif file.startswith('test_') and file.endswith('.py'):
                    test_file = os.path.join(root, file)
                    dest_file = os.path.join(tests_dir, file)
                    if not os.path.exists(dest_file):
                        shutil.move(test_file, dest_file)
                elif file.endswith('.md') or file.startswith('README'):
                    doc_file = os.path.join(root, file)
                    dest_file = os.path.join(docs_dir, file)
                    if not os.path.exists(dest_file):
                        shutil.move(doc_file, dest_file)

    return "Project structure organized."

def optimize_project(path):
    """Perform optimizations on the project."""
    # For now, just call normalize_code_style and remove_temporary_files
    # Can add more optimizations like removing unused imports, etc.
    normalize_code_style(path)
    remove_temporary_files(path)
    return "Project optimized."

def cleanup_project(path):
    """Perform all cleanup operations."""
    results = []
    results.append(remove_temporary_files(path))
    results.append(normalize_code_style(path))
    results.append(organize_project_structure(path))
    results.append(optimize_project(path))
    return "Cleanup completed. Removed: {}, Style: {}, Structure: {}, Optimized: {}".format(*results)