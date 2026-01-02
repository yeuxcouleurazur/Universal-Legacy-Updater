import os
import re
import logging

# Set up logging
logging.basicConfig(level=logging.WARNING)

# Known vulnerable and obsolete dependencies (expanded)
KNOWN_VULNERABLE = {
    'PHP': ['mysql', 'ereg', 'md5', 'sha1', 'eval', 'system', 'exec', 'passthru', 'shell_exec', 'proc_open', 'popen'],
    'Java': ['log4j:1.2.17', 'struts:2.3.0', 'spring-framework:4.0.0', 'jackson-databind:2.9.0'],
    'VB': ['mscomctl.ocx', 'mswinsock.ocx', 'comctl32.ocx'],
    'COBOL': ['db2', 'ims'],  # example vulnerable DB connections
    'Scripts': ['old_lib', 'telnet', 'ftp', 'rsh', 'rcp'],
    'Perl': ['unsafe', 'taint_mode_off'],
    'Fortran': ['implicit_none_missing'],
    'Ada': ['unchecked_conversion'],
    'Pascal': ['unsafe_pointers'],
    'Assembly': ['buffer_overflow'],
    'ASP': ['adodb', 'filesystemobject'],
    'ColdFusion': ['cfquery', 'cffile'],
    'ActionScript': ['flash_player_old'],
    'Objective-C': ['nsstring_format'],
    'Delphi': ['unsafe_cast'],
    'RPG': ['db2_old'],
    'Lisp': ['unsafe_eval', 'read_from_string'],
    'Scheme': ['unsafe_eval', 'read'],
    'Prolog': ['unsafe_meta', 'shell'],
    'Smalltalk': ['unsafe_eval', 'file_access'],
    'HyperTalk': ['old_hypercard'],
    'Tcl/Tk': ['eval', 'exec'],
    'VB.NET': ['unsafe_cast', 'old_framework'],
    'FoxPro': ['sql_injection', 'unsafe_dbf'],
    'Logo': ['unsafe_commands'],
    'Clipper': ['buffer_overflow', 'sql_injection'],
    'BASIC': ['goto_spaghetti', 'unsafe_input'],
    'Modula-2': ['unsafe_pointers'],
    'D': ['unsafe_gc', 'old_phobos'],
    'REXX': ['exec', 'unsafe_io'],
    'Erlang': ['unsafe_eval', 'old_otp']
}

KNOWN_OBSOLETE = {
    'PHP': ['php5', 'php7.0', 'php7.1', 'magic_quotes', 'register_globals'],
    'Java': ['java1.4', 'java1.5', 'java1.6', 'java1.7', 'applet', 'awt'],
    'VB': ['vb6', 'vbscript', 'activex'],
    'COBOL': ['cobol74', 'cobol85', 'jcl'],
    'Scripts': ['bash3', 'perl5.6', 'python2'],
    'Perl': ['perl5.6', 'perl5.8'],
    'Fortran': ['fortran77'],
    'Ada': ['ada83'],
    'Pascal': ['pascal7'],
    'Assembly': ['x86_old'],
    'ASP': ['asp3'],
    'ColdFusion': ['cf8'],
    'ActionScript': ['as2'],
    'Objective-C': ['objc1'],
    'Delphi': ['delphi7'],
    'RPG': ['rpgiii'],
    'Lisp': ['common_lisp_old'],
    'Scheme': ['scheme_old'],
    'Prolog': ['prolog_old'],
    'Smalltalk': ['smalltalk_old'],
    'HyperTalk': ['hypertalk'],
    'Tcl/Tk': ['tcl_old', 'tk_old'],
    'VB.NET': ['vb.net_old'],
    'FoxPro': ['foxpro_old'],
    'Logo': ['logo_old'],
    'Clipper': ['clipper_old'],
    'BASIC': ['basic_old'],
    'Modula-2': ['modula2'],
    'D': ['d1'],
    'REXX': ['rexx_old'],
    'Erlang': ['erlang_old']
}

def detect_language_from_content(content, ext):
    """Simple language detection based on content and extension."""
    if ext == '.php' and re.search(r'<\?php|\?>', content, re.IGNORECASE):
        return 'PHP'
    elif ext == '.vb' and re.search(r'\bDim\b|\bSub\b', content, re.IGNORECASE):
        return 'VB'
    elif ext == '.cbl' and re.search(r'IDENTIFICATION DIVISION', content, re.IGNORECASE):
        return 'COBOL'
    elif ext == '.java' and 'import' in content:
        return 'Java'
    elif ext in ['.sh', '.bat', '.pl'] and (content.startswith('#!') or '@echo' in content.lower()):
        return 'Scripts'
    elif ext == '.pl' and re.search(r'use\s+\w+;|sub\s+\w+', content):
        return 'Perl'
    elif ext in ['.f', '.f90'] and re.search(r'PROGRAM\s+\w+', content, re.IGNORECASE):
        return 'Fortran'
    elif ext == '.ada' and re.search(r'with\s+\w+;', content, re.IGNORECASE):
        return 'Ada'
    elif ext == '.pas' and re.search(r'program\s+\w+', content, re.IGNORECASE):
        return 'Pascal'
    elif ext == '.asm' and re.search(r'mov\s+|jmp\s+', content, re.IGNORECASE):
        return 'Assembly'
    elif ext == '.asp' and re.search(r'<%|%>|<script\s+language="vbscript"', content, re.IGNORECASE):
        return 'ASP'
    elif ext == '.cfm' and re.search(r'<cfset|<cfquery', content, re.IGNORECASE):
        return 'ColdFusion'
    elif ext == '.as' and re.search(r'package\s+\w+', content):
        return 'ActionScript'
    elif ext == '.m' and re.search(r'@interface|@implementation', content):
        return 'Objective-C'
    elif ext == '.dpr' and re.search(r'program\s+\w+', content, re.IGNORECASE):
        return 'Delphi'
    elif ext == '.rpg' and re.search(r'^H\s|^F\s', content, re.MULTILINE):
        return 'RPG'
    elif ext in ['.lisp', '.lsp'] and re.search(r'\(define|\(lambda', content):
        return 'Lisp'
    elif ext == '.scm' and re.search(r'\(define|\(lambda', content):
        return 'Scheme'
    elif ext == '.pl' and re.search(r':-|\w+\([^)]*\)\s*:-', content):
        return 'Prolog'
    elif ext == '.st' and re.search(r'\|\s*\w+\s*\|', content):
        return 'Smalltalk'
    elif ext == '.ht' and re.search(r'on\s+\w+', content, re.IGNORECASE):
        return 'HyperTalk'
    elif ext == '.tcl' and re.search(r'proc\s+\w+', content):
        return 'Tcl/Tk'
    elif ext == '.vb' and re.search(r'Imports\s+\w+', content):
        return 'VB.NET'
    elif ext in ['.prg', '.fox'] and re.search(r'USE\s+\w+', content, re.IGNORECASE):
        return 'FoxPro'
    elif ext == '.lgo' and re.search(r'to\s+\w+', content, re.IGNORECASE):
        return 'Logo'
    elif ext == '.prg' and re.search(r'FUNCTION\s+\w+', content, re.IGNORECASE):
        return 'Clipper'
    elif ext == '.bas' and re.search(r'DIM\s+\w+', content, re.IGNORECASE):
        return 'BASIC'
    elif ext in ['.mod', '.m2'] and re.search(r'MODULE\s+\w+', content, re.IGNORECASE):
        return 'Modula-2'
    elif ext == '.d' and re.search(r'import\s+\w+;', content):
        return 'D'
    elif ext in ['.rex', '.cmd'] and re.search(r'say\s+', content, re.IGNORECASE):
        return 'REXX'
    elif ext == '.erl' and re.search(r'-module\(', content):
        return 'Erlang'
    return None

def extract_dependencies(content, lang):
    """Extract dependencies based on language."""
    deps = []
    if lang == 'PHP':
        # Match require, include statements
        matches = re.findall(r'(?:require|include)(?:_once)?\s*\(?\s*[\'"]([^\'"]+)[\'"]\s*\)?', content, re.IGNORECASE)
        deps.extend(matches)
        # Match use statements for namespaces
        matches = re.findall(r'^use\s+([^;\s]+)', content, re.MULTILINE)
        deps.extend(matches)
        # Match class instantiations or static calls that might indicate dependencies
        matches = re.findall(r'new\s+([A-Z]\w+)', content)
        deps.extend(matches)
    elif lang == 'Java':
        # Match import statements
        matches = re.findall(r'^import\s+([^;]+);', content, re.MULTILINE)
        deps.extend(matches)
    elif lang == 'VB':
        # Match Declare for dll references
        matches = re.findall(r'Declare\s+.*\s+"([^"]+\.dll)"', content, re.IGNORECASE)
        deps.extend(matches)
        # Match references to external libraries
        matches = re.findall(r'References\s+([^,\s]+)', content, re.IGNORECASE)
        deps.extend(matches)
    elif lang == 'COBOL':
        # Match COPY statements
        matches = re.findall(r'COPY\s+([^.\s]+)', content, re.IGNORECASE)
        deps.extend(matches)
    elif lang == 'Scripts':
        # For bash: source or .
        matches = re.findall(r'(?:source|\.)\s+([^;\s]+)', content)
        deps.extend(matches)
        # For perl: use or require
        matches = re.findall(r'(?:use|require)\s+([^;\s]+)', content)
        deps.extend(matches)
        # For bash: curl or wget downloads
        matches = re.findall(r'(?:curl|wget)\s+[^\'"]*[\'"]([^\'"]+)[\'"]', content)
        deps.extend(matches)
        # For python-like scripts: import
        matches = re.findall(r'^import\s+([^;\s]+)|^from\s+([^;\s]+)', content, re.MULTILINE)
        deps.extend([m for match in matches for m in match if m])
    elif lang == 'Perl':
        # Match use, require
        matches = re.findall(r'(?:use|require)\s+([^;\s]+)', content)
        deps.extend(matches)
    elif lang == 'Fortran':
        # Match use
        matches = re.findall(r'use\s+([^,\s]+)', content, re.IGNORECASE)
        deps.extend(matches)
    elif lang == 'Ada':
        # Match with
        matches = re.findall(r'with\s+([^;\s]+)', content, re.IGNORECASE)
        deps.extend(matches)
    elif lang == 'Pascal':
        # Match uses
        matches = re.findall(r'uses\s+([^;\s]+)', content, re.IGNORECASE)
        deps.extend(matches)
    elif lang == 'Assembly':
        # Match include
        matches = re.findall(r'include\s+["\']([^"\']+)["\']', content, re.IGNORECASE)
        deps.extend(matches)
    elif lang == 'ASP':
        # Match include
        matches = re.findall(r'<!--#include\s+file=["\']([^"\']+)["\']', content, re.IGNORECASE)
        deps.extend(matches)
    elif lang == 'ColdFusion':
        # Match cfinclude
        matches = re.findall(r'<cfinclude\s+template=["\']([^"\']+)["\']', content, re.IGNORECASE)
        deps.extend(matches)
    elif lang == 'ActionScript':
        # Match import
        matches = re.findall(r'import\s+([^;\s]+)', content)
        deps.extend(matches)
    elif lang == 'Objective-C':
        # Match import
        matches = re.findall(r'#import\s+["<]([^">]+)[">]', content)
        deps.extend(matches)
    elif lang == 'Delphi':
        # Match uses
        matches = re.findall(r'uses\s+([^;\s]+)', content, re.IGNORECASE)
        deps.extend(matches)
    elif lang == 'RPG':
        # Match /copy
        matches = re.findall(r'/copy\s+([^,\s]+)', content, re.IGNORECASE)
        deps.extend(matches)
    elif lang == 'Lisp':
        # Match load, require
        matches = re.findall(r'\(load\s+["\']([^"\']+)["\']', content)
        deps.extend(matches)
        matches = re.findall(r'\(require\s+["\']([^"\']+)["\']', content)
        deps.extend(matches)
    elif lang == 'Scheme':
        # Similar to Lisp
        matches = re.findall(r'\(load\s+["\']([^"\']+)["\']', content)
        deps.extend(matches)
    elif lang == 'Prolog':
        # Match consult
        matches = re.findall(r'consult\(["\']([^"\']+)["\']', content)
        deps.extend(matches)
    elif lang == 'Smalltalk':
        # No clear dependency syntax, perhaps skip
        pass
    elif lang == 'HyperTalk':
        # No dependencies
        pass
    elif lang == 'Tcl/Tk':
        # Match source
        matches = re.findall(r'source\s+["\']([^"\']+)["\']', content)
        deps.extend(matches)
    elif lang == 'VB.NET':
        # Match Imports
        matches = re.findall(r'Imports\s+([^;\s]+)', content)
        deps.extend(matches)
    elif lang == 'FoxPro':
        # Match SET LIBRARY
        matches = re.findall(r'SET\s+LIBRARY\s+TO\s+([^,\s]+)', content, re.IGNORECASE)
        deps.extend(matches)
    elif lang == 'Logo':
        # No dependencies
        pass
    elif lang == 'Clipper':
        # No clear dependencies
        pass
    elif lang == 'BASIC':
        # No dependencies
        pass
    elif lang == 'Modula-2':
        # Match IMPORT
        matches = re.findall(r'IMPORT\s+([^;\s]+)', content, re.IGNORECASE)
        deps.extend(matches)
    elif lang == 'D':
        # Match import
        matches = re.findall(r'import\s+([^;\s]+)', content)
        deps.extend(matches)
    elif lang == 'REXX':
        # No dependencies
        pass
    elif lang == 'Erlang':
        # Match include
        matches = re.findall(r'-include\(["\']([^"\']+)["\']', content)
        deps.extend(matches)
    return deps

def check_dependency_status(dep, lang):
    """Check if dependency is obsolete or vulnerable."""
    dep_lower = dep.lower()
    vuln = KNOWN_VULNERABLE.get(lang, [])
    obs = KNOWN_OBSOLETE.get(lang, [])
    if any(v.lower() in dep_lower for v in vuln):
        return 'vulnerable'
    elif any(o.lower() in dep_lower for o in obs):
        return 'obsolete'
    else:
        return 'current'

def scan_for_vulnerabilities(content, lang, filepath):
    vulnerabilities = []
    def add_vuln(pattern, severity, description, flags=0):
        for match in re.finditer(pattern, content, flags):
            line_num = content[:match.start()].count('\n') + 1
            vulnerabilities.append({
                'severity': severity,
                'location': f'{filepath}:{line_num}',
                'description': description
            })

    if lang == 'PHP':
        # SQL injection
        add_vuln(r'mysql_query\s*\([^)]*\$.*[^)]*\)', 'high', 'Potential SQL injection: mysql_query with user input', re.IGNORECASE)
        # XSS
        add_vuln(r'echo\s+.*\$_GET|\$_POST', 'medium', 'Potential XSS: echoing user input', re.IGNORECASE)
        # Hardcoded credentials
        add_vuln(r'password.*=.*[\'"][^\'"]{3,}[\'"]', 'high', 'Hardcoded password', re.IGNORECASE)
        # Deprecated functions
        deprecated_funcs = ['mysql_connect', 'mysql_select_db', 'ereg', 'eregi']
        for func in deprecated_funcs:
            add_vuln(rf'\b{func}\s*\(', 'medium', f'Use of deprecated function {func}', re.IGNORECASE)

    elif lang == 'Java':
        # SQL injection
        add_vuln(r'Statement\s+\w+\s*=.*execute.*\+', 'high', 'Potential SQL injection: Statement with string concatenation')
        # Hardcoded
        add_vuln(r'String\s+\w*password\w*\s*=.*"[^"]+"', 'high', 'Hardcoded password', re.IGNORECASE)
        # XSS
        add_vuln(r'out\.println.*request\.getParameter', 'medium', 'Potential XSS: printing user input')

    elif lang == 'VB':
        # Hardcoded
        add_vuln(r'Dim\s+\w*password\w*\s+As\s+String\s*=.*"[^"]+"', 'high', 'Hardcoded password', re.IGNORECASE)
        # SQL injection
        add_vuln(r'Execute.*SELECT.*&', 'high', 'Potential SQL injection in VB')

    elif lang == 'COBOL':
        # SQL injection
        add_vuln(r'EXEC\s+SQL.*:.*END-EXEC', 'high', 'Potential SQL injection: EXEC SQL with host variables')

    elif lang == 'Scripts':
        # For bash: hardcoded
        add_vuln(r'PASSWORD\s*=.*[\'"][^\'"]+[\'"]', 'high', 'Hardcoded password in script', re.IGNORECASE)
        # For perl: similar
        add_vuln(r'my\s+\$password\s*=.*[\'"][^\'"]+[\'"]', 'high', 'Hardcoded password in Perl', re.IGNORECASE)

    elif lang == 'Perl':
        # Hardcoded
        add_vuln(r'my\s+\$password\s*=.*[\'"][^\'"]+[\'"]', 'high', 'Hardcoded password', re.IGNORECASE)
        # Eval
        add_vuln(r'eval\s*\(', 'high', 'Use of eval: security risk', re.IGNORECASE)

    elif lang == 'Fortran':
        # Buffer overflow potential
        add_vuln(r'CHARACTER\*', 'medium', 'Fixed length strings: potential buffer overflow')

    elif lang == 'Ada':
        # Unchecked conversion
        add_vuln(r'Unchecked_Conversion', 'high', 'Unchecked conversion: type safety issue', re.IGNORECASE)

    elif lang == 'Pascal':
        # Unsafe pointers
        add_vuln(r'\^', 'medium', 'Pointer usage: potential unsafe access')

    elif lang == 'Assembly':
        # No specific, but perhaps stack issues
        pass

    elif lang == 'ASP':
        # SQL injection
        add_vuln(r'Execute.*SELECT.*&', 'high', 'Potential SQL injection in ASP')

    elif lang == 'ColdFusion':
        # SQL injection
        add_vuln(r'<cfquery>.*#.*#', 'high', 'Potential SQL injection in ColdFusion')

    elif lang == 'ActionScript':
        # Hardcoded
        add_vuln(r'var\s+password\s*=.*["\'][^\'"]+["\']', 'high', 'Hardcoded password', re.IGNORECASE)

    elif lang == 'Objective-C':
        # Format string
        add_vuln(r'NSString\s+stringWithFormat', 'medium', 'Potential format string vulnerability')

    elif lang == 'Delphi':
        # Unsafe cast
        add_vuln(r'as\s+', 'medium', 'Unsafe cast', re.IGNORECASE)

    elif lang == 'RPG':
        # SQL injection
        add_vuln(r'EXEC\s+SQL.*:', 'high', 'Potential SQL injection in RPG')
    elif lang == 'Lisp':
        # Eval
        add_vuln(r'\(eval\s+', 'high', 'Use of eval: security risk', re.IGNORECASE)
    elif lang == 'Scheme':
        # Eval
        add_vuln(r'\(eval\s+', 'high', 'Use of eval: security risk', re.IGNORECASE)
    elif lang == 'Prolog':
        # Shell
        add_vuln(r'shell\(', 'high', 'Use of shell: command injection risk', re.IGNORECASE)
    elif lang == 'Smalltalk':
        # Eval
        add_vuln(r'Compiler\s+evaluate:', 'high', 'Dynamic evaluation: security risk', re.IGNORECASE)
    elif lang == 'Tcl/Tk':
        # Eval
        add_vuln(r'eval\s+', 'high', 'Use of eval: security risk', re.IGNORECASE)
    elif lang == 'VB.NET':
        # SQL injection
        add_vuln(r'ExecuteReader.*SELECT.*\+', 'high', 'Potential SQL injection in VB.NET')
    elif lang == 'FoxPro':
        # SQL injection
        add_vuln(r'SELECT.*&', 'high', 'Potential SQL injection in FoxPro')
    elif lang == 'D':
        # Unsafe operations
        add_vuln(r'cast\(', 'medium', 'Unsafe cast', re.IGNORECASE)
    elif lang == 'Erlang':
        # Eval
        add_vuln(r'erl_eval:', 'high', 'Use of erl_eval: security risk', re.IGNORECASE)

    return vulnerabilities

def detect_bad_practices(content, lang, filepath):
    bad_practices = []
    def add_bad_practice(pattern, severity, description, flags=0):
        for match in re.finditer(pattern, content, flags):
            line_num = content[:match.start()].count('\n') + 1
            bad_practices.append({
                'severity': severity,
                'location': f'{filepath}:{line_num}',
                'description': description
            })

    # Global variables
    if lang == 'PHP':
        add_bad_practice(r'\bglobal\s+\$[a-zA-Z_]\w*', 'medium', 'Use of global variables: consider encapsulation and dependency injection', re.IGNORECASE)
    elif lang == 'Java':
        add_bad_practice(r'public\s+static\s+\w+\s+\w+', 'medium', 'Public static field: potential global state, consider singleton or injection', re.IGNORECASE)
    elif lang == 'VB':
        add_bad_practice(r'\bGlobal\s+\w+', 'medium', 'Global variable: avoid global state', re.IGNORECASE)

    # Long functions (>50 lines)
    if lang == 'PHP':
        func_matches = re.finditer(r'function\s+\w+\s*\([^)]*\)\s*\{', content, re.IGNORECASE)
        for match in func_matches:
            start = match.end()
            brace_count = 1
            pos = start
            while pos < len(content) and brace_count > 0:
                if content[pos] == '{':
                    brace_count += 1
                elif content[pos] == '}':
                    brace_count -= 1
                pos += 1
            if brace_count == 0:
                func_body = content[start:pos-1]
                line_count = func_body.count('\n') + 1
                if line_count > 50:
                    line_num = content[:match.start()].count('\n') + 1
                    bad_practices.append({
                        'severity': 'medium',
                        'location': f'{filepath}:{line_num}',
                        'description': f'Long function: {line_count} lines. Consider breaking into smaller functions'
                    })

    # Magic numbers
    add_bad_practice(r'\b\d{3,}\b', 'low', 'Potential magic number: consider using named constants', re.IGNORECASE)

    # Poor naming
    if lang == 'PHP':
        add_bad_practice(r'\$[a-zA-Z]\b', 'low', 'Poor variable naming: use descriptive names instead of single letters', re.IGNORECASE)
    elif lang == 'Java':
        add_bad_practice(r'\b\w{1,2}\b\s*=', 'low', 'Poor variable naming: use descriptive names', re.IGNORECASE)

    # Lack of error handling (simple check: functions without try)
    if lang == 'PHP':
        func_matches = re.finditer(r'function\s+\w+\s*\([^)]*\)\s*\{', content, re.IGNORECASE)
        for match in func_matches:
            start = match.end()
            brace_count = 1
            pos = start
            while pos < len(content) and brace_count > 0:
                if content[pos] == '{':
                    brace_count += 1
                elif content[pos] == '}':
                    brace_count -= 1
                pos += 1
            if brace_count == 0:
                func_body = content[start:pos-1]
                if 'try' not in func_body:
                    line_num = content[:match.start()].count('\n') + 1
                    bad_practices.append({
                        'severity': 'low',
                        'location': f'{filepath}:{line_num}',
                        'description': 'Lack of error handling: consider adding try-catch blocks'
                    })

    # Language-specific bad practices
    if lang == 'PHP':
        add_bad_practice(r'\bvar\s+\$[a-zA-Z_]', 'low', 'Use of var keyword: use private/protected/public for visibility', re.IGNORECASE)
        add_bad_practice(r'\beval\s*\(', 'high', 'Use of eval: security risk, avoid dynamic code execution', re.IGNORECASE)
    elif lang == 'VB':
        add_bad_practice(r'\bGoTo\b', 'medium', 'Use of GoTo: leads to spaghetti code, use structured programming', re.IGNORECASE)
    elif lang == 'Java':
        add_bad_practice(r'System\.out\.println', 'low', 'Use of System.out.println: use logging framework instead', re.IGNORECASE)
    elif lang == 'Perl':
        add_bad_practice(r'\$&', 'medium', 'Use of $&: performance issue', re.IGNORECASE)
    elif lang == 'Fortran':
        add_bad_practice(r'IMPLICIT', 'medium', 'Implicit typing: use explicit', re.IGNORECASE)
    elif lang == 'Ada':
        add_bad_practice(r'pragma\s+Suppress', 'high', 'Suppress pragma: hides errors', re.IGNORECASE)
    elif lang == 'Pascal':
        add_bad_practice(r'goto', 'medium', 'Use of goto: structured programming', re.IGNORECASE)
    elif lang == 'Assembly':
        # Hard to detect bad practices
        pass
    elif lang == 'ASP':
        add_bad_practice(r'On\s+Error\s+Resume\s+Next', 'medium', 'Error suppression', re.IGNORECASE)
    elif lang == 'ColdFusion':
        add_bad_practice(r'<cfabort>', 'low', 'Abrupt termination', re.IGNORECASE)
    elif lang == 'ActionScript':
        add_bad_practice(r'trace\s*\(', 'low', 'Use of trace: use logging', re.IGNORECASE)
    elif lang == 'Objective-C':
        add_bad_practice(r'NSLog', 'low', 'Use of NSLog: use logging framework', re.IGNORECASE)
    elif lang == 'Delphi':
        add_bad_practice(r'goto', 'medium', 'Use of goto', re.IGNORECASE)
    elif lang == 'RPG':
        add_bad_practice(r'EXSR', 'low', 'Subroutine calls: use procedures', re.IGNORECASE)
    elif lang == 'Lisp':
        add_bad_practice(r'\(eval\s+', 'high', 'Use of eval: avoid dynamic code execution', re.IGNORECASE)
    elif lang == 'Scheme':
        add_bad_practice(r'\(eval\s+', 'high', 'Use of eval: avoid dynamic code execution', re.IGNORECASE)
    elif lang == 'Prolog':
        add_bad_practice(r'fail', 'low', 'Use of fail: consider better logic', re.IGNORECASE)
    elif lang == 'Smalltalk':
        add_bad_practice(r'whileTrue:', 'medium', 'Infinite loops: ensure termination', re.IGNORECASE)
    elif lang == 'Tcl/Tk':
        add_bad_practice(r'eval\s+', 'high', 'Use of eval: security risk', re.IGNORECASE)
    elif lang == 'VB.NET':
        add_bad_practice(r'Option\s+Explicit\s+Off', 'medium', 'Option Explicit Off: use explicit declarations', re.IGNORECASE)
    elif lang == 'FoxPro':
        add_bad_practice(r'GOTO', 'medium', 'Use of GOTO: leads to spaghetti code', re.IGNORECASE)
    elif lang == 'BASIC':
        add_bad_practice(r'GOTO', 'medium', 'Use of GOTO: leads to spaghetti code', re.IGNORECASE)
    elif lang == 'D':
        add_bad_practice(r'void\s+main', 'low', 'Use int main for standard compliance', re.IGNORECASE)
    elif lang == 'Erlang':
        add_bad_practice(r'catch', 'low', 'Use try-catch for better error handling', re.IGNORECASE)

    return bad_practices

def extract_business_logic(content, lang):
    """
    Extract core business logic from legacy code.
    Returns a structured summary including functions, classes, variables, control flows, and pseudocode.
    """
    summary = {
        'functions': [],
        'classes': [],
        'variables': [],
        'control_flows': [],
        'pseudocode': ''
    }

    if lang == 'PHP':
        # Functions
        func_pattern = r'function\s+(\w+)\s*\([^)]*\)\s*\{'
        for match in re.finditer(func_pattern, content, re.IGNORECASE):
            func_name = match.group(1)
            start = match.end()
            brace_count = 1
            pos = start
            while pos < len(content) and brace_count > 0:
                if content[pos] == '{':
                    brace_count += 1
                elif content[pos] == '}':
                    brace_count -= 1
                pos += 1
            func_body = content[start:pos-1]
            # Extract key logic for pseudocode
            pseudocode_lines = []
            lines = func_body.split('\n')
            for line in lines:
                line = line.strip()
                if line.startswith('if '):
                    pseudocode_lines.append(f"if {line[3:].split('{')[0].strip()}")
                elif line.startswith('for ') or line.startswith('foreach '):
                    pseudocode_lines.append(f"loop over {line.split('(')[1].split(')')[0] if '(' in line else line}")
                elif line.startswith('while '):
                    pseudocode_lines.append(f"while {line[6:].split('{')[0].strip()}")
                elif '=' in line and not line.startswith('//'):
                    pseudocode_lines.append(f"assign {line}")
                elif line.startswith('return '):
                    pseudocode_lines.append(f"return {line[7:]}")
            pseudocode = f"function {func_name}():\n" + '\n'.join(f"  {line}" for line in pseudocode_lines[:5])  # Limit to 5 lines
            summary['functions'].append({
                'name': func_name,
                'body': func_body,
                'pseudocode': pseudocode
            })

        # Classes
        class_pattern = r'class\s+(\w+)'
        for match in re.finditer(class_pattern, content, re.IGNORECASE):
            class_name = match.group(1)
            summary['classes'].append({
                'name': class_name,
                'body': '',  # Could extract body similarly
                'pseudocode': f"class {class_name}"
            })

        # Variables (global)
        var_pattern = r'global\s+\$([a-zA-Z_]\w*)'
        for match in re.finditer(var_pattern, content, re.IGNORECASE):
            var_name = match.group(1)
            summary['variables'].append({
                'name': f"${var_name}",
                'type': 'global'
            })

        # Control flows
        cf_patterns = [
            (r'if\s*\([^)]*\)\s*\{', 'if'),
            (r'for\s*\([^)]*\)\s*\{', 'for'),
            (r'while\s*\([^)]*\)\s*\{', 'while'),
            (r'foreach\s*\([^)]*\)\s*\{', 'foreach')
        ]
        for pattern, type_ in cf_patterns:
            for match in re.finditer(pattern, content, re.IGNORECASE):
                summary['control_flows'].append({
                    'type': type_,
                    'body': match.group(0)
                })

    elif lang == 'Java':
        # Functions (methods)
        func_pattern = r'(?:public|private|protected)?\s*\w+\s+(\w+)\s*\([^)]*\)\s*\{'
        for match in re.finditer(func_pattern, content):
            func_name = match.group(1)
            start = match.end()
            brace_count = 1
            pos = start
            while pos < len(content) and brace_count > 0:
                if content[pos] == '{':
                    brace_count += 1
                elif content[pos] == '}':
                    brace_count -= 1
                pos += 1
            func_body = content[match.start():pos]
            summary['functions'].append({
                'name': func_name,
                'body': func_body,
                'pseudocode': f"method {func_name}() {{ {func_body.replace('{', '').replace('}', '').strip()} }}"
            })

        # Classes
        class_pattern = r'class\s+(\w+)'
        for match in re.finditer(class_pattern, content):
            class_name = match.group(1)
            summary['classes'].append({
                'name': class_name,
                'pseudocode': f"class {class_name}"
            })

        # Variables (fields)
        var_pattern = r'(?:public|private|protected)?\s*\w+\s+(\w+)\s*;'
        for match in re.finditer(var_pattern, content):
            var_name = match.group(1)
            summary['variables'].append({
                'name': var_name,
                'type': 'field'
            })

        # Control flows
        cf_patterns = [
            (r'if\s*\([^)]*\)\s*\{', 'if'),
            (r'for\s*\([^)]*\)\s*\{', 'for'),
            (r'while\s*\([^)]*\)\s*\{', 'while')
        ]
        for pattern, type_ in cf_patterns:
            for match in re.finditer(pattern, content):
                summary['control_flows'].append({
                    'type': type_,
                    'body': match.group(0)
                })

    elif lang == 'VB':
        # Functions (Subs/Functions)
        func_pattern = r'(?:Public|Private)?\s*(?:Sub|Function)\s+(\w+)\s*\([^)]*\)'
        for match in re.finditer(func_pattern, content, re.IGNORECASE):
            func_name = match.group(1)
            # VB uses End Sub/Function, harder to extract body with regex
            summary['functions'].append({
                'name': func_name,
                'body': '',
                'pseudocode': f"sub {func_name}()"
            })

        # Classes
        class_pattern = r'Class\s+(\w+)'
        for match in re.finditer(class_pattern, content, re.IGNORECASE):
            class_name = match.group(1)
            summary['classes'].append({
                'name': class_name,
                'pseudocode': f"class {class_name}"
            })

        # Variables (Dim)
        var_pattern = r'Dim\s+(\w+)\s+As\s+\w+'
        for match in re.finditer(var_pattern, content, re.IGNORECASE):
            var_name = match.group(1)
            summary['variables'].append({
                'name': var_name,
                'type': 'dim'
            })

        # Control flows
        cf_patterns = [
            (r'If\s+.*\s+Then', 'if'),
            (r'For\s+.*\s+To', 'for'),
            (r'While\s+', 'while')
        ]
        for pattern, type_ in cf_patterns:
            for match in re.finditer(pattern, content, re.IGNORECASE):
                summary['control_flows'].append({
                    'type': type_,
                    'body': match.group(0)
                })

    elif lang == 'COBOL':
        # Procedures (paragraphs)
        proc_pattern = r'(\w+)\.'
        for match in re.finditer(proc_pattern, content):
            proc_name = match.group(1)
            summary['functions'].append({
                'name': proc_name,
                'body': '',
                'pseudocode': f"procedure {proc_name}"
            })

        # No classes in COBOL
        # Variables (data divisions)
        var_pattern = r'\d+\s+(\w+)\s+PIC'
        for match in re.finditer(var_pattern, content, re.IGNORECASE):
            var_name = match.group(1)
            summary['variables'].append({
                'name': var_name,
                'type': 'pic'
            })

        # Control flows
        cf_patterns = [
            (r'IF\s+', 'if'),
            (r'PERFORM\s+', 'perform'),
            (r'LOOP\s+', 'loop')
        ]
        for pattern, type_ in cf_patterns:
            for match in re.finditer(pattern, content, re.IGNORECASE):
                summary['control_flows'].append({
                    'type': type_,
                    'body': match.group(0)
                })

    elif lang == 'Scripts':
        # For bash/perl, functions
        if 'function' in content or 'sub' in content:
            func_pattern = r'(?:function\s+)?(\w+)\s*\(\)\s*\{|sub\s+(\w+)\s*\{'
            for match in re.finditer(func_pattern, content):
                func_name = match.group(1) or match.group(2)
                summary['functions'].append({
                    'name': func_name,
                    'body': '',
                    'pseudocode': f"function {func_name}()"
                })

        # Variables
        var_pattern = r'(\w+)\s*='
        for match in re.finditer(var_pattern, content):
            var_name = match.group(1)
            if not var_name.startswith('$') and not var_name in ['if', 'for', 'while']:
                summary['variables'].append({
                    'name': var_name,
                    'type': 'variable'
                })

        # Control flows
        cf_patterns = [
            (r'if\s+\[', 'if'),
            (r'for\s+', 'for'),
            (r'while\s+', 'while')
        ]
        for pattern, type_ in cf_patterns:
            for match in re.finditer(pattern, content):
                summary['control_flows'].append({
                    'type': type_,
                    'body': match.group(0)
                })

    elif lang == 'Perl':
        # Subs
        sub_pattern = r'sub\s+(\w+)'
        for match in re.finditer(sub_pattern, content):
            sub_name = match.group(1)
            summary['functions'].append({
                'name': sub_name,
                'body': '',
                'pseudocode': f"sub {sub_name}()"
            })

        # Variables
        var_pattern = r'my\s+\$(\w+)'
        for match in re.finditer(var_pattern, content):
            var_name = match.group(1)
            summary['variables'].append({
                'name': f"${var_name}",
                'type': 'my'
            })

        # Control flows
        cf_patterns = [
            (r'if\s*\(', 'if'),
            (r'for\s*\(', 'for'),
            (r'while\s*\(', 'while')
        ]
        for pattern, type_ in cf_patterns:
            for match in re.finditer(pattern, content):
                summary['control_flows'].append({
                    'type': type_,
                    'body': match.group(0)
                })

    elif lang == 'Fortran':
        # Functions/Subroutines
        func_pattern = r'(?:FUNCTION|SUBROUTINE)\s+(\w+)'
        for match in re.finditer(func_pattern, content, re.IGNORECASE):
            func_name = match.group(1)
            summary['functions'].append({
                'name': func_name,
                'body': '',
                'pseudocode': f"function {func_name}()"
            })

        # Variables
        var_pattern = r'(\w+)\s*='
        for match in re.finditer(var_pattern, content):
            var_name = match.group(1)
            if var_name.upper() not in ['IF', 'DO', 'WHILE']:
                summary['variables'].append({
                    'name': var_name,
                    'type': 'variable'
                })

        # Control flows
        cf_patterns = [
            (r'IF\s*\(', 'if'),
            (r'DO\s+', 'do'),
            (r'WHILE\s*\(', 'while')
        ]
        for pattern, type_ in cf_patterns:
            for match in re.finditer(pattern, content, re.IGNORECASE):
                summary['control_flows'].append({
                    'type': type_,
                    'body': match.group(0)
                })

    elif lang == 'Ada':
        # Procedures/Functions
        proc_pattern = r'(?:procedure|function)\s+(\w+)'
        for match in re.finditer(proc_pattern, content, re.IGNORECASE):
            proc_name = match.group(1)
            summary['functions'].append({
                'name': proc_name,
                'body': '',
                'pseudocode': f"procedure {proc_name}()"
            })

        # Variables
        var_pattern = r'(\w+)\s*:'
        for match in re.finditer(var_pattern, content):
            var_name = match.group(1)
            summary['variables'].append({
                'name': var_name,
                'type': 'variable'
            })

        # Control flows
        cf_patterns = [
            (r'if\s+', 'if'),
            (r'for\s+', 'for'),
            (r'while\s+', 'while')
        ]
        for pattern, type_ in cf_patterns:
            for match in re.finditer(pattern, content, re.IGNORECASE):
                summary['control_flows'].append({
                    'type': type_,
                    'body': match.group(0)
                })

    elif lang == 'Pascal':
        # Procedures/Functions
        proc_pattern = r'(?:procedure|function)\s+(\w+)'
        for match in re.finditer(proc_pattern, content, re.IGNORECASE):
            proc_name = match.group(1)
            summary['functions'].append({
                'name': proc_name,
                'body': '',
                'pseudocode': f"procedure {proc_name}()"
            })

        # Variables
        var_pattern = r'(\w+)\s*:'
        for match in re.finditer(var_pattern, content):
            var_name = match.group(1)
            summary['variables'].append({
                'name': var_name,
                'type': 'variable'
            })

        # Control flows
        cf_patterns = [
            (r'if\s+', 'if'),
            (r'for\s+', 'for'),
            (r'while\s+', 'while')
        ]
        for pattern, type_ in cf_patterns:
            for match in re.finditer(pattern, content, re.IGNORECASE):
                summary['control_flows'].append({
                    'type': type_,
                    'body': match.group(0)
                })

    elif lang == 'Assembly':
        # Labels as functions
        label_pattern = r'(\w+):'
        for match in re.finditer(label_pattern, content):
            label_name = match.group(1)
            summary['functions'].append({
                'name': label_name,
                'body': '',
                'pseudocode': f"label {label_name}"
            })

    elif lang == 'ASP':
        # Subs/Functions
        func_pattern = r'(?:Sub|Function)\s+(\w+)'
        for match in re.finditer(func_pattern, content, re.IGNORECASE):
            func_name = match.group(1)
            summary['functions'].append({
                'name': func_name,
                'body': '',
                'pseudocode': f"sub {func_name}()"
            })

        # Variables
        var_pattern = r'Dim\s+(\w+)'
        for match in re.finditer(var_pattern, content, re.IGNORECASE):
            var_name = match.group(1)
            summary['variables'].append({
                'name': var_name,
                'type': 'dim'
            })

        # Control flows
        cf_patterns = [
            (r'If\s+', 'if'),
            (r'For\s+', 'for'),
            (r'While\s+', 'while')
        ]
        for pattern, type_ in cf_patterns:
            for match in re.finditer(pattern, content, re.IGNORECASE):
                summary['control_flows'].append({
                    'type': type_,
                    'body': match.group(0)
                })

    elif lang == 'ColdFusion':
        # Functions
        func_pattern = r'<cffunction\s+name="([^"]+)"'
        for match in re.finditer(func_pattern, content, re.IGNORECASE):
            func_name = match.group(1)
            summary['functions'].append({
                'name': func_name,
                'body': '',
                'pseudocode': f"function {func_name}()"
            })

        # Variables
        var_pattern = r'<cfset\s+(\w+)'
        for match in re.finditer(var_pattern, content, re.IGNORECASE):
            var_name = match.group(1)
            summary['variables'].append({
                'name': var_name,
                'type': 'cfset'
            })

        # Control flows
        cf_patterns = [
            (r'<cfif\s+', 'if'),
            (r'<cfloop\s+', 'loop')
        ]
        for pattern, type_ in cf_patterns:
            for match in re.finditer(pattern, content, re.IGNORECASE):
                summary['control_flows'].append({
                    'type': type_,
                    'body': match.group(0)
                })

    elif lang == 'ActionScript':
        # Functions
        func_pattern = r'function\s+(\w+)'
        for match in re.finditer(func_pattern, content):
            func_name = match.group(1)
            summary['functions'].append({
                'name': func_name,
                'body': '',
                'pseudocode': f"function {func_name}()"
            })

        # Variables
        var_pattern = r'var\s+(\w+)'
        for match in re.finditer(var_pattern, content):
            var_name = match.group(1)
            summary['variables'].append({
                'name': var_name,
                'type': 'var'
            })

        # Control flows
        cf_patterns = [
            (r'if\s*\(', 'if'),
            (r'for\s*\(', 'for'),
            (r'while\s*\(', 'while')
        ]
        for pattern, type_ in cf_patterns:
            for match in re.finditer(pattern, content):
                summary['control_flows'].append({
                    'type': type_,
                    'body': match.group(0)
                })

    elif lang == 'Objective-C':
        # Methods
        method_pattern = r'-\s*\([^)]+\)\s*(\w+)'
        for match in re.finditer(method_pattern, content):
            method_name = match.group(1)
            summary['functions'].append({
                'name': method_name,
                'body': '',
                'pseudocode': f"method {method_name}()"
            })

        # Variables
        var_pattern = r'(\w+)\s*='
        for match in re.finditer(var_pattern, content):
            var_name = match.group(1)
            if not var_name in ['if', 'for', 'while']:
                summary['variables'].append({
                    'name': var_name,
                    'type': 'variable'
                })

        # Control flows
        cf_patterns = [
            (r'if\s*\(', 'if'),
            (r'for\s*\(', 'for'),
            (r'while\s*\(', 'while')
        ]
        for pattern, type_ in cf_patterns:
            for match in re.finditer(pattern, content):
                summary['control_flows'].append({
                    'type': type_,
                    'body': match.group(0)
                })

    elif lang == 'Delphi':
        # Procedures/Functions
        proc_pattern = r'(?:procedure|function)\s+(\w+)'
        for match in re.finditer(proc_pattern, content, re.IGNORECASE):
            proc_name = match.group(1)
            summary['functions'].append({
                'name': proc_name,
                'body': '',
                'pseudocode': f"procedure {proc_name}()"
            })

        # Variables
        var_pattern = r'(\w+)\s*:'
        for match in re.finditer(var_pattern, content):
            var_name = match.group(1)
            summary['variables'].append({
                'name': var_name,
                'type': 'variable'
            })

        # Control flows
        cf_patterns = [
            (r'if\s+', 'if'),
            (r'for\s+', 'for'),
            (r'while\s+', 'while')
        ]
        for pattern, type_ in cf_patterns:
            for match in re.finditer(pattern, content, re.IGNORECASE):
                summary['control_flows'].append({
                    'type': type_,
                    'body': match.group(0)
                })

    elif lang == 'RPG':
        # Procedures
        proc_pattern = r'P\s+(\w+)'
        for match in re.finditer(proc_pattern, content):
            proc_name = match.group(1)
            summary['functions'].append({
                'name': proc_name,
                'body': '',
                'pseudocode': f"procedure {proc_name}"
            })

        # Variables
        var_pattern = r'D\s+(\w+)'
        for match in re.finditer(var_pattern, content):
            var_name = match.group(1)
            summary['variables'].append({
                'name': var_name,
                'type': 'd-spec'
            })

        # Control flows
        cf_patterns = [
            (r'IF\s+', 'if'),
            (r'DO\s+', 'do')
        ]
        for pattern, type_ in cf_patterns:
            for match in re.finditer(pattern, content):
                summary['control_flows'].append({
                    'type': type_,
                    'body': match.group(0)
                })

    elif lang == 'Lisp':
        # Functions
        func_pattern = r'\(defun\s+(\w+)'
        for match in re.finditer(func_pattern, content, re.IGNORECASE):
            func_name = match.group(1)
            summary['functions'].append({
                'name': func_name,
                'body': '',
                'pseudocode': f"defun {func_name}()"
            })

        # Variables
        var_pattern = r'\(defvar\s+(\w+)'
        for match in re.finditer(var_pattern, content, re.IGNORECASE):
            var_name = match.group(1)
            summary['variables'].append({
                'name': var_name,
                'type': 'defvar'
            })

        # Control flows
        cf_patterns = [
            (r'\(if\s+', 'if'),
            (r'\(loop\s+', 'loop')
        ]
        for pattern, type_ in cf_patterns:
            for match in re.finditer(pattern, content):
                summary['control_flows'].append({
                    'type': type_,
                    'body': match.group(0)
                })

    elif lang == 'Scheme':
        # Similar to Lisp
        func_pattern = r'\(define\s+\(\s*(\w+)'
        for match in re.finditer(func_pattern, content):
            func_name = match.group(1)
            summary['functions'].append({
                'name': func_name,
                'body': '',
                'pseudocode': f"define ({func_name})"
            })

    elif lang == 'Prolog':
        # Predicates
        pred_pattern = r'(\w+)\([^)]*\)\s*:-'
        for match in re.finditer(pred_pattern, content):
            pred_name = match.group(1)
            summary['functions'].append({
                'name': pred_name,
                'body': '',
                'pseudocode': f"predicate {pred_name}"
            })

    elif lang == 'Smalltalk':
        # Methods
        method_pattern = r'(\w+):\s*\w+'
        for match in re.finditer(method_pattern, content):
            method_name = match.group(1)
            summary['functions'].append({
                'name': method_name,
                'body': '',
                'pseudocode': f"method {method_name}"
            })

    elif lang == 'Tcl/Tk':
        # Procedures
        proc_pattern = r'proc\s+(\w+)'
        for match in re.finditer(proc_pattern, content):
            proc_name = match.group(1)
            summary['functions'].append({
                'name': proc_name,
                'body': '',
                'pseudocode': f"proc {proc_name}"
            })

    elif lang == 'VB.NET':
        # Subs/Functions
        func_pattern = r'(?:Public|Private)?\s*(?:Sub|Function)\s+(\w+)'
        for match in re.finditer(func_pattern, content, re.IGNORECASE):
            func_name = match.group(1)
            summary['functions'].append({
                'name': func_name,
                'body': '',
                'pseudocode': f"sub {func_name}()"
            })

    elif lang == 'FoxPro':
        # Functions
        func_pattern = r'FUNCTION\s+(\w+)'
        for match in re.finditer(func_pattern, content, re.IGNORECASE):
            func_name = match.group(1)
            summary['functions'].append({
                'name': func_name,
                'body': '',
                'pseudocode': f"function {func_name}"
            })

    elif lang == 'BASIC':
        # Subs
        sub_pattern = r'SUB\s+(\w+)'
        for match in re.finditer(sub_pattern, content, re.IGNORECASE):
            sub_name = match.group(1)
            summary['functions'].append({
                'name': sub_name,
                'body': '',
                'pseudocode': f"sub {sub_name}"
            })

    elif lang == 'Modula-2':
        # Procedures
        proc_pattern = r'PROCEDURE\s+(\w+)'
        for match in re.finditer(proc_pattern, content, re.IGNORECASE):
            proc_name = match.group(1)
            summary['functions'].append({
                'name': proc_name,
                'body': '',
                'pseudocode': f"procedure {proc_name}"
            })

    elif lang == 'D':
        # Functions
        func_pattern = r'\w+\s+(\w+)\s*\('
        for match in re.finditer(func_pattern, content):
            func_name = match.group(1)
            if func_name not in ['if', 'for', 'while']:
                summary['functions'].append({
                    'name': func_name,
                    'body': '',
                    'pseudocode': f"function {func_name}()"
                })

    elif lang == 'REXX':
        # No clear functions
        pass

    elif lang == 'Erlang':
        # Functions
        func_pattern = r'(\w+)\s*\('
        for match in re.finditer(func_pattern, content):
            func_name = match.group(1)
            if func_name not in ['if', 'case']:
                summary['functions'].append({
                    'name': func_name,
                    'body': '',
                    'pseudocode': f"function {func_name}()"
                })

    # Generate overall pseudocode as a concatenation of key parts
    pseudocode_parts = []
    for func in summary['functions']:
        pseudocode_parts.append(func['pseudocode'])
    for cls in summary['classes']:
        pseudocode_parts.append(cls['pseudocode'])
    summary['pseudocode'] = '\n'.join(pseudocode_parts)

    return summary

def analyze_code(path):
    """
    Analyze the codebase for structure, dependencies, etc.
    """
    total_files = 0
    total_lines = 0
    dependencies = []
    vulnerabilities = []
    bad_practices = []
    for root, dirs, files in os.walk(path):
        for file in files:
            ext = os.path.splitext(file)[1].lower()
            if ext in ['.php', '.vb', '.cbl', '.java', '.sh', '.bat', '.pl', '.f', '.f90', '.ada', '.pas', '.asm', '.asp', '.cfm', '.as', '.m', '.dpr', '.rpg', '.lisp', '.lsp', '.scm', '.st', '.ht', '.tcl', '.prg', '.fox', '.lgo', '.bas', '.mod', '.m2', '.d', '.rex', '.cmd', '.erl']:
                total_files += 1
                filepath = os.path.join(root, file)
                try:
                    with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
                        content = f.read()
                        lines = content.splitlines()
                        total_lines += len(lines)
                        lang = detect_language_from_content(content, ext)
                        if lang:
                            deps = extract_dependencies(content, lang)
                            for dep in deps:
                                status = check_dependency_status(dep, lang)
                                dependencies.append({
                                    'file': filepath,
                                    'language': lang,
                                    'dependency': dep,
                                    'status': status
                                })
                            vulns = scan_for_vulnerabilities(content, lang, filepath)
                            vulnerabilities.extend(vulns)
                            bad_practs = detect_bad_practices(content, lang, filepath)
                            bad_practices.extend(bad_practs)
                except Exception as e:
                    logging.warning(f"Error analyzing file {filepath}: {e}")
    return {"total_files": total_files, "total_lines": total_lines, "dependencies": dependencies, "vulnerabilities": vulnerabilities, "bad_practices": bad_practices}