import os
import re
from typing import Dict, List, Tuple

def scan_secrets(path: str) -> List[Dict[str, str]]:
    """
    Scan for hardcoded secrets in the given path.
    Returns a list of dictionaries with file, line, type, and value.
    """
    secrets = []
    secret_patterns = {
        'api_key': r'(?i)(api[_-]?key|apikey)\s*[:=]\s*["\']([^"\']+)["\']',
        'password': r'(?i)(password|passwd|pwd)\s*[:=]\s*["\']([^"\']+)["\']',
        'token': r'(?i)(token|secret|auth[_-]?key)\s*[:=]\s*["\']([^"\']+)["\']',
        'aws_key': r'(?i)(aws[_-]?access[_-]?key[_-]?id|aws[_-]?secret[_-]?access[_-]?key)\s*[:=]\s*["\']([^"\']+)["\']',
        'private_key': r'(?i)-----BEGIN\s+(RSA\s+)?PRIVATE\s+KEY-----',
    }

    for root, dirs, files in os.walk(path):
        for file in files:
            if file.endswith(('.py', '.php', '.java', '.js', '.vb', '.cbl', '.bat', '.sh', '.pl')):
                filepath = os.path.join(root, file)
                try:
                    with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
                        lines = f.readlines()
                        for line_num, line in enumerate(lines, 1):
                            for secret_type, pattern in secret_patterns.items():
                                matches = re.findall(pattern, line)
                                for match in matches:
                                    if isinstance(match, tuple):
                                        value = match[-1]  # Last group is the value
                                    else:
                                        value = match
                                    secrets.append({
                                        'file': filepath,
                                        'line': str(line_num),
                                        'type': secret_type,
                                        'value': value
                                    })
                except Exception as e:
                    print(f"Error reading {filepath}: {e}")
    return secrets

def scan_vulnerabilities(path: str) -> List[Dict[str, str]]:
    """
    Scan for common security vulnerabilities in the given path.
    Returns a list of dictionaries with file, line, type, and description.
    """
    vulnerabilities = []
    vuln_patterns = {
        'eval_usage': r'\beval\s*\(',
        'os_system': r'os\.system\s*\(',
        'subprocess_shell': r'subprocess\.(call|Popen|run)\s*\([^)]*shell\s*=\s*True',
        'sql_injection': r'(SELECT|INSERT|UPDATE|DELETE).*WHERE.*\+.*\$',  # Simple pattern for PHP-like
        'xss_potential': r'echo\s+.*\$_',  # PHP echo with variables
    }

    for root, dirs, files in os.walk(path):
        for file in files:
            if file.endswith(('.py', '.php', '.java', '.js', '.vb', '.cbl', '.bat', '.sh', '.pl')):
                filepath = os.path.join(root, file)
                try:
                    with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
                        lines = f.readlines()
                        for line_num, line in enumerate(lines, 1):
                            for vuln_type, pattern in vuln_patterns.items():
                                if re.search(pattern, line):
                                    vulnerabilities.append({
                                        'file': filepath,
                                        'line': str(line_num),
                                        'type': vuln_type,
                                        'description': f"Potential {vuln_type.replace('_', ' ')} vulnerability"
                                    })
                except Exception as e:
                    print(f"Error reading {filepath}: {e}")
    return vulnerabilities

def generate_security_report(secrets: List[Dict[str, str]], vulnerabilities: List[Dict[str, str]], path: str) -> str:
    """
    Generate a security report in Markdown format.
    """
    report = f"# Security Analysis Report for {path}\n\n"

    report += "## Hardcoded Secrets\n\n"
    if secrets:
        for secret in secrets:
            report += f"- **File:** {secret['file']}:{secret['line']}\n"
            report += f"  **Type:** {secret['type']}\n"
            report += f"  **Value:** {secret['value']}\n\n"
    else:
        report += "No hardcoded secrets detected.\n\n"

    report += "## Security Vulnerabilities\n\n"
    if vulnerabilities:
        for vuln in vulnerabilities:
            report += f"- **File:** {vuln['file']}:{vuln['line']}\n"
            report += f"  **Type:** {vuln['type']}\n"
            report += f"  **Description:** {vuln['description']}\n\n"
    else:
        report += "No security vulnerabilities detected.\n\n"

    return report