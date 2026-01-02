import os
import shutil

def initialize_project(directory='.'):
    """Initialize a new migration project structure."""
    dirs = ['src', 'tests', 'docs', 'samples']
    files = {
        'docs/README.md': '# Migration Project\n\nThis is a migration project initialized with ULU.',
        'ulu.ini': '[ulu]\n# Configuration file for ULU\n'
    }

    for d in dirs:
        path = os.path.join(directory, d)
        if not os.path.exists(path):
            os.makedirs(path)
            print(f"Created directory: {path}")
        else:
            print(f"Directory already exists: {path}")

    for f, content in files.items():
        path = os.path.join(directory, f)
        if not os.path.exists(path):
            with open(path, 'w') as file:
                file.write(content)
            print(f"Created file: {path}")
        else:
            print(f"File already exists: {path}")

    print("Project initialized successfully.")