import os
import zipfile
import tarfile
import shutil

def package_zip(source_path, output_path):
    """
    Package the source directory into a ZIP archive.

    :param source_path: Path to the directory to package
    :param output_path: Path to the output ZIP file
    """
    with zipfile.ZipFile(output_path, 'w', zipfile.ZIP_DEFLATED) as zipf:
        for root, dirs, files in os.walk(source_path):
            for file in files:
                file_path = os.path.join(root, file)
                arcname = os.path.relpath(file_path, source_path)
                zipf.write(file_path, arcname)
    return f"Packaged {source_path} into {output_path}"

def package_tar(source_path, output_path):
    """
    Package the source directory into a TAR.GZ archive.

    :param source_path: Path to the directory to package
    :param output_path: Path to the output TAR.GZ file
    """
    with tarfile.open(output_path, 'w:gz') as tarf:
        tarf.add(source_path, arcname=os.path.basename(source_path))
    return f"Packaged {source_path} into {output_path}"

def package_docker(source_path, output_path):
    """
    Generate a Docker image setup by creating a Dockerfile and copying the source.

    :param source_path: Path to the directory to package
    :param output_path: Path to the output directory for Docker artifacts
    """
    os.makedirs(output_path, exist_ok=True)
    # Copy source to output
    dest_path = os.path.join(output_path, 'app')
    if os.path.exists(dest_path):
        shutil.rmtree(dest_path)
    shutil.copytree(source_path, dest_path)
    # Generate Dockerfile
    dockerfile_content = """FROM python:3.9-slim

WORKDIR /app

COPY . .

# Assuming it's a Python app, install requirements if present
RUN if [ -f requirements.txt ]; then pip install -r requirements.txt; fi

CMD ["python", "main.py"]
"""
    dockerfile_path = os.path.join(output_path, 'Dockerfile')
    with open(dockerfile_path, 'w') as f:
        f.write(dockerfile_content)
    return f"Generated Docker setup in {output_path}"

def package_project(source_path, format_type, output_path):
    """
    Package the project based on the format.

    :param source_path: Path to the project directory
    :param format_type: 'zip', 'tar', or 'docker'
    :param output_path: Output path
    """
    if format_type == 'zip':
        return package_zip(source_path, output_path)
    elif format_type == 'tar':
        return package_tar(source_path, output_path)
    elif format_type == 'docker':
        return package_docker(source_path, output_path)
    else:
        raise ValueError(f"Unsupported format: {format_type}")