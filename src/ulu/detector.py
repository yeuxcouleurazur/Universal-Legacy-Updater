import os
import re
import javalang

def detect_php(content):
    if re.search(r'<\?php|\?>', content, re.IGNORECASE):
        # detect version
        match = re.search(r'PHP\s+(\d+\.\d+)', content, re.IGNORECASE)
        if match:
            return match.group(1)
        # check features
        if 'namespace' in content:
            return '5.3+'
        elif '??' in content:
            return '7.0+'
        elif '::' in content and 'class' in content:
            return '5.5+'
        else:
            return '5'
    return None

def detect_vb(content):
    if re.search(r'\bDim\b|\bSub\b|\bEnd Sub\b', content, re.IGNORECASE):
        # assume VB6 for legacy
        return '6'
    return None

def detect_cobol(content):
    if re.search(r'IDENTIFICATION DIVISION|PROCEDURE DIVISION', content, re.IGNORECASE):
        if 'OBJECT-ORIENTED' in content:
            return '2002'
        elif 'EVALUATE' in content:
            return '85'
        else:
            return '74'
    return None

def detect_java(content):
    try:
        javalang.parse.parse(content)
        # detect version
        if re.search(r'<\w+>', content):  # generics
            return '5+'
        elif '@Override' in content:
            return '5+'
        elif 'enum' in content:
            return '5+'
        else:
            return '1.4'
    except:
        return None

def detect_perl(content):
    if re.search(r'#!/usr/bin/perl|use\s+\w+;', content) or re.search(r'sub\s+\w+', content):
        # Assume modern Perl
        return '5.30'
    return None

def detect_fortran(content):
    if re.search(r'PROGRAM\s+\w+|FUNCTION\s+\w+|SUBROUTINE\s+\w+', content, re.IGNORECASE):
        if re.search(r'IMPLICIT\s+NONE', content, re.IGNORECASE):
            return '90+'
        else:
            return '77'
    return None

def detect_ada(content):
    if re.search(r'with\s+\w+;|procedure\s+\w+|function\s+\w+', content, re.IGNORECASE):
        return '95+'
    return None

def detect_pascal(content):
    if re.search(r'program\s+\w+|procedure\s+\w+|function\s+\w+', content, re.IGNORECASE):
        return '7.0'
    return None

def detect_assembly(content):
    if re.search(r'\.data|\.text|mov\s+|jmp\s+', content, re.IGNORECASE):
        return 'x86'
    return None

def detect_asp(content):
    if re.search(r'<%|%>|<script\s+language="vbscript"', content, re.IGNORECASE):
        return '3.0'
    return None

def detect_coldfusion(content):
    if re.search(r'<cfset|<cfquery|<cfoutput', content, re.IGNORECASE):
        return '9'
    return None

def detect_actionscript(content):
    if re.search(r'package\s+\w+|class\s+\w+|function\s+\w+', content) and re.search(r'var\s+\w+:', content):
        return '3.0'
    return None

def detect_objectivec(content):
    if re.search(r'@interface|@implementation|@end', content):
        return '2.0'
    return None

def detect_delphi(content):
    if re.search(r'unit\s+\w+;|uses\s+\w+;|procedure\s+\w+|function\s+\w+', content, re.IGNORECASE):
        return '7'
    return None

def detect_rpg(content):
    if re.search(r'^H\s|^F\s|^D\s|^P\s', content, re.MULTILINE):
        return 'ILE'
    return None

def detect_script(content):
    lines = content.split('\n', 1)[0]
    if lines.startswith('#!'):
        if 'bash' in lines:
            return 'bash'
        elif 'perl' in lines:
            return 'perl'
        elif 'python' in lines:
            return 'python'
        else:
            return 'script'
    elif re.search(r'@echo|rem', content, re.IGNORECASE):
        return 'batch'
    return None

def detect_lisp(content):
    if re.search(r'\(defun|\(define|\(lambda|\(let|\(if', content):
        return 'Common Lisp'
    return None

def detect_scheme(content):
    if re.search(r'\(define|\(lambda|\(let|\(if', content) and re.search(r'#t|#f', content):
        return 'Scheme'
    return None

def detect_prolog(content):
    if re.search(r':-|\.\s*$', content) and re.search(r'\w+\([^)]*\)\s*:-', content):
        return 'Prolog'
    return None

def detect_smalltalk(content):
    if re.search(r'\|\s*\w+\s*\|', content) and re.search(r'self|super', content):
        return 'Smalltalk'
    return None

def detect_hypertalk(content):
    if re.search(r'on\s+\w+|end\s+\w+', content, re.IGNORECASE):
        return 'HyperTalk'
    return None

def detect_tcl(content):
    if re.search(r'proc\s+\w+|set\s+\w+', content):
        return 'Tcl/Tk'
    return None

def detect_vbnet(content):
    if re.search(r'Imports\s+\w+|Namespace\s+\w+', content) and re.search(r'\.NET|System\.', content):
        return 'VB.NET'
    return None

def detect_foxpro(content):
    if re.search(r'USE\s+\w+|SELECT\s+\w+', content, re.IGNORECASE):
        return 'FoxPro'
    return None

def detect_logo(content):
    if re.search(r'to\s+\w+|forward|right|left', content, re.IGNORECASE):
        return 'Logo'
    return None

def detect_clipper(content):
    if re.search(r'FUNCTION\s+\w+|PROCEDURE\s+\w+', content, re.IGNORECASE):
        return 'Clipper'
    return None

def detect_basic(content):
    if re.search(r'DIM\s+\w+|PRINT\s+', content, re.IGNORECASE) and not re.search(r'Imports|Namespace', content):
        return 'BASIC'
    return None

def detect_modula2(content):
    if re.search(r'MODULE\s+\w+|PROCEDURE\s+\w+', content, re.IGNORECASE):
        return 'Modula-2'
    return None

def detect_old_d(content):
    if re.search(r'import\s+\w+;|void\s+main\s*\(', content) and re.search(r'printf|scanf', content):
        return 'D1'
    return None

def detect_rexx(content):
    if re.search(r'say\s+|pull\s+', content, re.IGNORECASE):
        return 'REXX'
    return None

def detect_old_erlang(content):
    if re.search(r'-module\(|-export\(', content):
        return 'Erlang'
    return None

def detect_language(path):
    """
    Detect legacy languages and their versions in the given path from file contents.
    Returns a dict of language to version.
    """
    languages = {}
    for root, dirs, files in os.walk(path):
        for file in files:
            filepath = os.path.join(root, file)
            try:
                with open(filepath, 'r', encoding='utf-8') as f:
                    content = f.read()
            except:
                continue  # skip binary or unreadable files
            # detect languages
            for lang, detect_func in [('PHP', detect_php), ('VB', detect_vb), ('COBOL', detect_cobol), ('Java', detect_java), ('Script', detect_script), ('Perl', detect_perl), ('Fortran', detect_fortran), ('Ada', detect_ada), ('Pascal', detect_pascal), ('Assembly', detect_assembly), ('ASP', detect_asp), ('ColdFusion', detect_coldfusion), ('ActionScript', detect_actionscript), ('Objective-C', detect_objectivec), ('Delphi', detect_delphi), ('RPG', detect_rpg), ('Lisp', detect_lisp), ('Scheme', detect_scheme), ('Prolog', detect_prolog), ('Smalltalk', detect_smalltalk), ('HyperTalk', detect_hypertalk), ('Tcl/Tk', detect_tcl), ('VB.NET', detect_vbnet), ('FoxPro', detect_foxpro), ('Logo', detect_logo), ('Clipper', detect_clipper), ('BASIC', detect_basic), ('Modula-2', detect_modula2), ('D', detect_old_d), ('REXX', detect_rexx), ('Erlang', detect_old_erlang)]:
                version = detect_func(content)
                if version:
                    languages[lang] = version
    return languages