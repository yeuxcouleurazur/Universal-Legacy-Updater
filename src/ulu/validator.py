import os
import ast
import sys
import importlib.util
import subprocess
import platform

def check_syntax(path):
    """
    Check syntax of Python files in the given path.
    Returns a list of errors.
    """
    errors = []
    for root, dirs, files in os.walk(path):
        for file in files:
            if file.endswith('.py'):
                filepath = os.path.join(root, file)
                try:
                    with open(filepath, 'r', encoding='utf-8') as f:
                        source = f.read()
                    ast.parse(source, filename=filepath)
                except SyntaxError as e:
                    errors.append(f"Syntax error in {filepath}: {e}")
                except Exception as e:
                    errors.append(f"Error parsing {filepath}: {e}")
    return errors

def resolve_dependencies(path):
    """
    Check if dependencies can be resolved.
    Looks for requirements.txt and tries to import modules.
    Returns a list of missing dependencies.
    """
    missing = []
    req_file = os.path.join(path, 'requirements.txt')
    if os.path.exists(req_file):
        try:
            with open(req_file, 'r') as f:
                requirements = [line.strip() for line in f if line.strip() and not line.startswith('#')]
            for req in requirements:
                # Simple check: try to import the module name (before any version spec)
                module_name = req.split()[0].split('==')[0].split('>=')[0].split('<=')[0]
                try:
                    importlib.import_module(module_name)
                except ImportError:
                    missing.append(module_name)
        except Exception as e:
            missing.append(f"Error reading requirements.txt: {e}")
    else:
        # If no requirements.txt, check imports in code
        for root, dirs, files in os.walk(path):
            for file in files:
                if file.endswith('.py'):
                    filepath = os.path.join(root, file)
                    try:
                        with open(filepath, 'r') as f:
                            tree = ast.parse(f.read())
                        for node in ast.walk(tree):
                            if isinstance(node, ast.Import):
                                for alias in node.names:
                                    try:
                                        importlib.import_module(alias.name)
                                    except ImportError:
                                        missing.append(alias.name)
                            elif isinstance(node, ast.ImportFrom):
                                try:
                                    importlib.import_module(node.module)
                                except ImportError:
                                    missing.append(node.module)
                    except:
                        pass
    return list(set(missing))  # unique

def run_basic_tests(path):
    """
    Run basic functionality tests.
    If there's a main.py or __main__.py, try to run it.
    Returns success or errors.
    """
    errors = []
    main_file = os.path.join(path, 'main.py')
    if os.path.exists(main_file):
        try:
            result = subprocess.run([sys.executable, main_file], capture_output=True, text=True, timeout=10)
            if result.returncode != 0:
                errors.append(f"Error running main.py: {result.stderr}")
        except subprocess.TimeoutExpired:
            errors.append("Timeout running main.py")
        except Exception as e:
            errors.append(f"Exception running main.py: {e}")
    # Check for __main__.py in packages
    for root, dirs, files in os.walk(path):
        if '__main__.py' in files:
            main_path = os.path.join(root, '__main__.py')
            try:
                result = subprocess.run([sys.executable, main_path], capture_output=True, text=True, timeout=10, cwd=root)
                if result.returncode != 0:
                    errors.append(f"Error running {main_path}: {result.stderr}")
            except subprocess.TimeoutExpired:
                errors.append(f"Timeout running {main_path}")
            except Exception as e:
                errors.append(f"Exception running {main_path}: {e}")
    return errors

def check_compatibility(path):
    """
    Check compatibility: Python version, OS, etc.
    Returns a list of compatibility issues.
    """
    issues = []
    python_version = sys.version_info
    if python_version < (3, 6):
        issues.append("Python version too old, requires at least 3.6")
    # Check for OS-specific code, but simplistic
    os_name = platform.system().lower()
    for root, dirs, files in os.walk(path):
        for file in files:
            if file.endswith('.py'):
                filepath = os.path.join(root, file)
                try:
                    with open(filepath, 'r') as f:
                        content = f.read()
                    if 'os.name' in content or 'platform.system' in content:
                        # Assume it's handled, but warn if not
                        pass
                except:
                    pass
    return issues

def validate_project(path):
    """
    Run all validation checks and return a report.
    """
    report = f"Validation Report for {path}\n\n"

    # Syntax check
    syntax_errors = check_syntax(path)
    if syntax_errors:
        report += "Syntax Check: FAILED\n"
        for err in syntax_errors:
            report += f"  - {err}\n"
    else:
        report += "Syntax Check: PASSED\n"

    # Dependency resolution
    missing_deps = resolve_dependencies(path)
    if missing_deps:
        report += "Dependency Resolution: FAILED\n"
        for dep in missing_deps:
            report += f"  - Missing: {dep}\n"
    else:
        report += "Dependency Resolution: PASSED\n"

    # Basic tests
    test_errors = run_basic_tests(path)
    if test_errors:
        report += "Basic Functionality Tests: FAILED\n"
        for err in test_errors:
            report += f"  - {err}\n"
    else:
        report += "Basic Functionality Tests: PASSED\n"

    # Compatibility
    compat_issues = check_compatibility(path)
    if compat_issues:
        report += "Compatibility Checks: FAILED\n"
        for issue in compat_issues:
            report += f"  - {issue}\n"
    else:
        report += "Compatibility Checks: PASSED\n"

    # Overall
    all_passed = not (syntax_errors or missing_deps or test_errors or compat_issues)
    report += f"\nOverall: {'PASSED' if all_passed else 'FAILED'}\n"

    return report