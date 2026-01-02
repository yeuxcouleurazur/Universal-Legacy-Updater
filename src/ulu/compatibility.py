import os
import sys
import platform
import ast
import re
from typing import Dict, List, Tuple

def check_os_compatibility(project_path: str) -> Dict[str, List[str]]:
    """
    Check OS compatibility by analyzing code for OS-specific constructs.
    Returns a dict with OS names as keys and lists of issues as values.
    """
    issues = {
        'windows': [],
        'linux': [],
        'macos': [],
        'cross_platform': []
    }

    for root, dirs, files in os.walk(project_path):
        for file in files:
            if file.endswith('.py'):
                filepath = os.path.join(root, file)
                try:
                    with open(filepath, 'r', encoding='utf-8') as f:
                        content = f.read()
                        _analyze_os_specific_code(content, issues)
                except Exception as e:
                    issues['cross_platform'].append(f"Error reading {filepath}: {e}")

    return issues

def _analyze_os_specific_code(content: str, issues: Dict[str, List[str]]):
    """Analyze code for OS-specific patterns."""
    # Check for os.name checks
    if 'os.name' in content:
        issues['cross_platform'].append("Uses os.name for OS detection - ensure cross-platform compatibility")

    # Check for platform.system()
    if 'platform.system()' in content:
        issues['cross_platform'].append("Uses platform.system() - verify handling of all platforms")

    # Check for Windows-specific paths
    if re.search(r'\\\\|C:\\\\', content):
        issues['windows'].append("Contains Windows-specific path separators")

    # Check for Unix-specific paths
    if re.search(r'/usr/|/etc/', content):
        issues['linux'].append("Contains Linux-specific paths")

    # Check for macOS-specific imports or code
    if 'AppKit' in content or 'Foundation' in content:
        issues['macos'].append("Uses macOS-specific frameworks")

def check_python_version(project_path: str) -> Dict[str, str]:
    """
    Check Python version requirements by analyzing code features.
    Returns dict with min_version and issues.
    """
    min_version = '3.6'  # Default minimum
    issues = []

    features = {
        'f-strings': '3.6',
        'walrus_operator': '3.8',
        'positional_only_args': '3.8',
        'dataclasses': '3.7',
        'typing_extensions': '3.7'  # For some advanced typing
    }

    for root, dirs, files in os.walk(project_path):
        for file in files:
            if file.endswith('.py'):
                filepath = os.path.join(root, file)
                try:
                    with open(filepath, 'r', encoding='utf-8') as f:
                        content = f.read()
                        version_req = _analyze_python_features(content)
                        if version_req and _compare_versions(version_req, min_version) > 0:
                            min_version = version_req
                except Exception as e:
                    issues.append(f"Error analyzing {filepath}: {e}")

    return {
        'min_version': min_version,
        'issues': issues
    }

def _analyze_python_features(content: str) -> str:
    """Analyze code for Python version-specific features."""
    if 'f"' in content or "f'" in content:
        return '3.6'
    if ':=' in content:
        return '3.8'
    if 'dataclasses' in content:
        return '3.7'
    return None

def _compare_versions(v1: str, v2: str) -> int:
    """Compare two version strings. Return 1 if v1 > v2, -1 if v1 < v2, 0 if equal."""
    v1_parts = [int(x) for x in v1.split('.')]
    v2_parts = [int(x) for x in v2.split('.')]
    for i in range(max(len(v1_parts), len(v2_parts))):
        v1_part = v1_parts[i] if i < len(v1_parts) else 0
        v2_part = v2_parts[i] if i < len(v2_parts) else 0
        if v1_part > v2_part:
            return 1
        elif v1_part < v2_part:
            return -1
    return 0

def check_library_conflicts(project_path: str) -> Dict[str, List[str]]:
    """
    Check for library version conflicts by analyzing requirements.txt and imports.
    Returns dict with conflicts and missing dependencies.
    """
    conflicts = []
    missing = []

    requirements_file = os.path.join(project_path, 'requirements.txt')
    if os.path.exists(requirements_file):
        try:
            with open(requirements_file, 'r') as f:
                requirements = f.read().splitlines()
                # Simple conflict check: look for multiple versions of same package
                packages = {}
                for req in requirements:
                    if '==' in req:
                        package, version = req.split('==', 1)
                        package = package.strip()
                        if package in packages:
                            conflicts.append(f"Multiple versions for {package}: {packages[package]} and {version}")
                        else:
                            packages[package] = version
        except Exception as e:
            conflicts.append(f"Error reading requirements.txt: {e}")
    else:
        missing.append("requirements.txt not found")

    # Check for imports that might indicate missing dependencies
    imports = set()
    for root, dirs, files in os.walk(project_path):
        for file in files:
            if file.endswith('.py'):
                filepath = os.path.join(root, file)
                try:
                    with open(filepath, 'r', encoding='utf-8') as f:
                        tree = ast.parse(f.read())
                        for node in ast.walk(tree):
                            if isinstance(node, ast.Import):
                                for alias in node.names:
                                    imports.add(alias.name.split('.')[0])
                            elif isinstance(node, ast.ImportFrom):
                                if node.module:
                                    imports.add(node.module.split('.')[0])
                except Exception as e:
                    pass  # Skip files that can't be parsed

    # Common external libraries that might need checking
    common_libs = {'requests', 'flask', 'django', 'numpy', 'pandas', 'tensorflow', 'torch'}
    for lib in common_libs:
        if lib in imports and lib not in packages:
            missing.append(f"Imported {lib} but not in requirements.txt")

    return {
        'conflicts': conflicts,
        'missing': missing
    }

def check_environment_issues(project_path: str) -> List[str]:
    """
    Check for environment-specific issues like missing files, permissions, etc.
    """
    issues = []

    # Check for common config files
    config_files = ['.env', 'config.json', 'settings.py']
    for config in config_files:
        if not os.path.exists(os.path.join(project_path, config)):
            issues.append(f"Missing configuration file: {config}")

    # Check for executable permissions on scripts
    for root, dirs, files in os.walk(project_path):
        for file in files:
            if file.endswith(('.sh', '.bat', '.py')) and file != '__init__.py':
                filepath = os.path.join(root, file)
                if os.name == 'posix' and not os.access(filepath, os.X_OK):
                    issues.append(f"Script {filepath} may need execute permissions")

    # Check for hardcoded paths
    for root, dirs, files in os.walk(project_path):
        for file in files:
            if file.endswith('.py'):
                filepath = os.path.join(root, file)
                try:
                    with open(filepath, 'r', encoding='utf-8') as f:
                        content = f.read()
                        if '/hardcoded/path' in content or 'C:\\hardcoded' in content:
                            issues.append(f"Hardcoded paths in {filepath}")
                except:
                    pass

    return issues

def analyze_compatibility(project_path: str, target_environments: List[str] = None) -> Dict[str, Dict]:
    """
    Main function to analyze compatibility for different target environments.
    target_environments can include 'windows', 'linux', 'macos', 'python3.8', 'python3.9', etc.
    """
    if target_environments is None:
        target_environments = ['windows', 'linux', 'macos', 'python3.8', 'python3.9', 'python3.10']

    results = {}

    # OS compatibility
    os_issues = check_os_compatibility(project_path)
    for env in ['windows', 'linux', 'macos']:
        if env in target_environments:
            results[env] = {
                'status': 'compatible' if not os_issues[env] else 'issues_found',
                'issues': os_issues[env]
            }

    # Python version
    py_version = check_python_version(project_path)
    for env in target_environments:
        if env.startswith('python'):
            version = env.split('python')[1]
            compatible = _compare_versions(py_version['min_version'], version) <= 0
            results[env] = {
                'status': 'compatible' if compatible else 'incompatible',
                'min_required': py_version['min_version'],
                'issues': py_version['issues']
            }

    # Library conflicts
    lib_conflicts = check_library_conflicts(project_path)
    results['libraries'] = {
        'status': 'ok' if not lib_conflicts['conflicts'] and not lib_conflicts['missing'] else 'issues_found',
        'conflicts': lib_conflicts['conflicts'],
        'missing': lib_conflicts['missing']
    }

    # Environment issues
    env_issues = check_environment_issues(project_path)
    results['environment'] = {
        'status': 'ok' if not env_issues else 'issues_found',
        'issues': env_issues
    }

    return results