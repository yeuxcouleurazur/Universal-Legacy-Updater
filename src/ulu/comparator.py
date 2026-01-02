import os
import re
import difflib
from .analyzer import extract_business_logic, detect_language_from_content

def compare_file_structures(legacy_path, modernized_path):
    """
    Compare file structures between legacy and modernized projects.
    Returns a dict with added, removed, modified files.
    """
    def get_file_list(path):
        files = []
        for root, dirs, files_in_dir in os.walk(path):
            for file in files_in_dir:
                rel_path = os.path.relpath(os.path.join(root, file), path)
                files.append(rel_path)
        return set(files)

    legacy_files = get_file_list(legacy_path)
    modernized_files = get_file_list(modernized_path)

    added = modernized_files - legacy_files
    removed = legacy_files - modernized_files
    common = legacy_files & modernized_files

    modified = []
    for file in common:
        legacy_file = os.path.join(legacy_path, file)
        modernized_file = os.path.join(modernized_path, file)
        try:
            with open(legacy_file, 'r', encoding='utf-8', errors='ignore') as f:
                legacy_content = f.read()
            with open(modernized_file, 'r', encoding='utf-8', errors='ignore') as f:
                modernized_content = f.read()
            if legacy_content != modernized_content:
                modified.append(file)
        except:
            modified.append(file)  # If can't read, consider modified

    return {
        'added': list(added),
        'removed': list(removed),
        'modified': modified,
        'unchanged': list(common - set(modified))
    }

def compare_code_metrics(legacy_path, modernized_path):
    """
    Compare code metrics: lines of code, complexity.
    """
    def calculate_metrics(path):
        total_loc = 0
        total_complexity = 0
        file_count = 0
        for root, dirs, files in os.walk(path):
            for file in files:
                ext = os.path.splitext(file)[1].lower()
                if ext in ['.php', '.vb', '.cbl', '.java', '.sh', '.bat', '.pl', '.py']:
                    filepath = os.path.join(root, file)
                    try:
                        with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
                            content = f.read()
                            lines = content.splitlines()
                            loc = len([line for line in lines if line.strip() and not line.strip().startswith('//') and not line.strip().startswith('#')])
                            total_loc += loc
                            # Simple complexity: count control structures
                            complexity = len(re.findall(r'\b(if|for|while|foreach|case|when)\b', content, re.IGNORECASE))
                            total_complexity += complexity
                            file_count += 1
                    except:
                        pass
        return {'loc': total_loc, 'complexity': total_complexity, 'files': file_count}

    legacy_metrics = calculate_metrics(legacy_path)
    modernized_metrics = calculate_metrics(modernized_path)

    return {
        'legacy': legacy_metrics,
        'modernized': modernized_metrics,
        'diff': {
            'loc_diff': modernized_metrics['loc'] - legacy_metrics['loc'],
            'complexity_diff': modernized_metrics['complexity'] - legacy_metrics['complexity'],
            'files_diff': modernized_metrics['files'] - legacy_metrics['files']
        }
    }

def compare_business_logic(legacy_path, modernized_path):
    """
    Compare preserved business logic.
    """
    def extract_all_business_logic(path):
        logic = {'functions': [], 'classes': [], 'variables': [], 'control_flows': []}
        for root, dirs, files in os.walk(path):
            for file in files:
                ext = os.path.splitext(file)[1].lower()
                if ext in ['.php', '.vb', '.cbl', '.java', '.sh', '.bat', '.pl', '.py']:
                    filepath = os.path.join(root, file)
                    try:
                        with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
                            content = f.read()
                            lang = detect_language_from_content(content, ext)
                            if lang:
                                bl = extract_business_logic(content, lang)
                                for key in logic:
                                    logic[key].extend(bl.get(key, []))
                    except:
                        pass
        return logic

    legacy_logic = extract_all_business_logic(legacy_path)
    modernized_logic = extract_all_business_logic(modernized_path)

    preserved = {}
    for key in legacy_logic:
        legacy_items = set(str(item) for item in legacy_logic[key])
        modernized_items = set(str(item) for item in modernized_logic[key])
        preserved[key] = {
            'preserved': list(legacy_items & modernized_items),
            'added': list(modernized_items - legacy_items),
            'removed': list(legacy_items - modernized_items)
        }

    return preserved

def generate_diff_report(legacy_path, modernized_path):
    """
    Generate a diff-style report comparing legacy and modernized code.
    """
    report = []

    # File structures
    structures = compare_file_structures(legacy_path, modernized_path)
    report.append("=== File Structure Comparison ===")
    report.append(f"Added files: {len(structures['added'])}")
    for f in structures['added'][:10]:  # Limit
        report.append(f"  + {f}")
    if len(structures['added']) > 10:
        report.append(f"  ... and {len(structures['added']) - 10} more")
    report.append(f"Removed files: {len(structures['removed'])}")
    for f in structures['removed'][:10]:
        report.append(f"  - {f}")
    if len(structures['removed']) > 10:
        report.append(f"  ... and {len(structures['removed']) - 10} more")
    report.append(f"Modified files: {len(structures['modified'])}")
    for f in structures['modified'][:10]:
        report.append(f"  ~ {f}")
    if len(structures['modified']) > 10:
        report.append(f"  ... and {len(structures['modified']) - 10} more")
    report.append("")

    # Code metrics
    metrics = compare_code_metrics(legacy_path, modernized_path)
    report.append("=== Code Metrics Comparison ===")
    report.append(f"Legacy: LOC={metrics['legacy']['loc']}, Complexity={metrics['legacy']['complexity']}, Files={metrics['legacy']['files']}")
    report.append(f"Modernized: LOC={metrics['modernized']['loc']}, Complexity={metrics['modernized']['complexity']}, Files={metrics['modernized']['files']}")
    report.append(f"Differences: LOC {metrics['diff']['loc_diff']:+d}, Complexity {metrics['diff']['complexity_diff']:+d}, Files {metrics['diff']['files_diff']:+d}")
    report.append("")

    # Business logic
    logic = compare_business_logic(legacy_path, modernized_path)
    report.append("=== Business Logic Preservation ===")
    for key, data in logic.items():
        report.append(f"{key.capitalize()}: Preserved {len(data['preserved'])}, Added {len(data['added'])}, Removed {len(data['removed'])}")
        if data['preserved']:
            report.append(f"  Preserved: {', '.join(data['preserved'][:5])}")
        if data['removed']:
            report.append(f"  Removed: {', '.join(data['removed'][:5])}")
        if data['added']:
            report.append(f"  Added: {', '.join(data['added'][:5])}")
        report.append("")

    # Detailed diffs for modified files
    report.append("=== Detailed Diffs (first 3 modified files) ===")
    for file in structures['modified'][:3]:
        legacy_file = os.path.join(legacy_path, file)
        modernized_file = os.path.join(modernized_path, file)
        try:
            with open(legacy_file, 'r', encoding='utf-8', errors='ignore') as f:
                legacy_content = f.read()
            with open(modernized_file, 'r', encoding='utf-8', errors='ignore') as f:
                modernized_content = f.read()
            diff = list(difflib.unified_diff(legacy_content.splitlines(), modernized_content.splitlines(), fromfile=file, tofile=file, lineterm=''))
            if diff:
                report.append(f"Diff for {file}:")
                report.extend(diff[:20])  # Limit lines
                report.append("")
        except:
            report.append(f"Could not generate diff for {file}")
            report.append("")

    return "\n".join(report)