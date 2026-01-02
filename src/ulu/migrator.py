import os
import shutil
import logging
import re
from .analyzer import analyze_code, extract_business_logic, detect_language_from_content

logging.basicConfig(level=logging.WARNING)

# Migration suggestions for each legacy language
MIGRATION_SUGGESTIONS = {
    'PHP': {
        'modern_frameworks': ['Laravel', 'Symfony', 'CodeIgniter', 'Node.js with Express'],
        'patterns': ['MVC (Model-View-Controller)', 'Clean Architecture', 'Dependency Injection'],
        'dependency_replacements': {
            'mysql': 'PDO or mysqli (modern)',
            'ereg': 'preg_match (PCRE)',
            'php5': 'PHP 8.x'
        },
        'code_examples': {
            'old': "<?php mysql_query('SELECT * FROM users'); ?>",
            'new': "<?php $pdo = new PDO('mysql:host=localhost;dbname=test', $user, $pass);\n$stmt = $pdo->query('SELECT * FROM users');\nwhile ($row = $stmt->fetch()) { echo $row['name']; } ?>"
        },
        'risks': ['Potential breaking changes in PHP versions', 'Framework learning curve', 'Database abstraction changes']
    },
    'VB': {
        'modern_frameworks': ['C# with .NET', 'VB.NET'],
        'patterns': ['MVC', 'MVVM (Model-View-ViewModel)', 'Object-Oriented Programming'],
        'dependency_replacements': {
            'mscomctl.ocx': '.NET controls',
            'vb6': 'VB.NET'
        },
        'code_examples': {
            'old': "Dim x As Integer\nx = 5\nMsgBox x",
            'new': "Dim x As Integer = 5\nMessageBox.Show(x.ToString())"
        },
        'risks': ['UI redesign required', 'COM interop issues', 'Performance differences']
    },
    'COBOL': {
        'modern_frameworks': ['Java with Spring', 'Python with Django', 'C# with .NET'],
        'patterns': ['Microservices', 'RESTful APIs', 'Domain-Driven Design'],
        'dependency_replacements': {
            'cobol74': 'Modern COBOL or Java equivalents'
        },
        'code_examples': {
            'old': "IDENTIFICATION DIVISION.\nPROGRAM-ID. HELLO.\nPROCEDURE DIVISION.\nDISPLAY 'Hello World'.\nSTOP RUN.",
            'new': "public class Hello {\n    public static void main(String[] args) {\n        System.out.println(\"Hello World\");\n    }\n}"
        },
        'risks': ['Complete rewrite often necessary', 'Data format changes', 'Mainframe to cloud migration complexity']
    },
    'Java': {
        'modern_frameworks': ['Spring Boot', 'Quarkus', 'Micronaut'],
        'patterns': ['Microservices', 'Reactive Programming', 'Hexagonal Architecture'],
        'dependency_replacements': {
            'log4j:1.2.17': 'log4j 2.x or SLF4J with Logback',
            'java1.4': 'Java 11+'
        },
        'code_examples': {
            'old': "import java.sql.*;\nStatement stmt = conn.createStatement();\nResultSet rs = stmt.executeQuery(\"SELECT * FROM users WHERE id=\" + userId);",
            'new': "import java.sql.*;\nPreparedStatement stmt = conn.prepareStatement(\"SELECT * FROM users WHERE id = ?\");\nstmt.setInt(1, userId);\nResultSet rs = stmt.executeQuery();"
        },
        'risks': ['JVM version compatibility', 'Library updates may require code changes', 'Performance tuning']
    },
    'Scripts': {
        'modern_frameworks': ['Python with Flask/Django', 'Node.js', 'Go'],
        'patterns': ['Modular scripting', 'Configuration management', 'Containerization'],
        'dependency_replacements': {
            'old_lib': 'Modern equivalents'
        },
        'code_examples': {
            'old': "#!/bin/bash\necho 'Hello World'",
            'new': "#!/usr/bin/env python3\nprint('Hello World')"
        },
        'risks': ['Shell compatibility issues', 'Security improvements may break automation', 'Learning new languages']
    },
    'Perl': {
        'modern_frameworks': ['Python', 'Ruby', 'Go'],
        'patterns': ['Object-Oriented Programming', 'Web frameworks', 'Modern syntax'],
        'dependency_replacements': {
            'perl5.6': 'Perl 5.30+'
        },
        'code_examples': {
            'old': "#!/usr/bin/perl\nprint 'Hello World';",
            'new': "#!/usr/bin/env python3\nprint('Hello World')"
        },
        'risks': ['Syntax differences', 'Module availability', 'Performance changes']
    },
    'Fortran': {
        'modern_frameworks': ['C++', 'Python with NumPy', 'Julia'],
        'patterns': ['Modern Fortran standards', 'Interoperability with C', 'Parallel computing'],
        'dependency_replacements': {
            'fortran77': 'Fortran 90+'
        },
        'code_examples': {
            'old': "PROGRAM HELLO\nWRITE(*,*) 'Hello World'\nEND PROGRAM",
            'new': "program hello\n    write(*,*) 'Hello World'\nend program hello"
        },
        'risks': ['Numerical precision changes', 'Library migration', 'Learning curve']
    },
    'Ada': {
        'modern_frameworks': ['C++', 'Rust', 'Go'],
        'patterns': ['Object-Oriented features', 'Concurrency', 'Safety'],
        'dependency_replacements': {
            'ada83': 'Ada 95+'
        },
        'code_examples': {
            'old': "with Ada.Text_IO; procedure Hello is begin Ada.Text_IO.Put_Line('Hello World'); end Hello;",
            'new': "with Ada.Text_IO; use Ada.Text_IO;\nprocedure Hello is\nbegin\n    Put_Line('Hello World');\nend Hello;"
        },
        'risks': ['Strict typing may require changes', 'Runtime differences', 'Toolchain updates']
    },
    'Pascal': {
        'modern_frameworks': ['C#', 'Java', 'Python'],
        'patterns': ['Object Pascal', 'Modern Pascal compilers', 'Cross-platform development'],
        'dependency_replacements': {
            'pascal7': 'Free Pascal or Delphi'
        },
        'code_examples': {
            'old': "program Hello;\nbegin\nwriteln('Hello World');\nend.",
            'new': "program Hello;\nbegin\n    writeln('Hello World');\nend."
        },
        'risks': ['Syntax modernization', 'Library changes', 'IDE migration']
    },
    'Assembly': {
        'modern_frameworks': ['C/C++', 'Rust', 'Go'],
        'patterns': ['Inline assembly', 'High-level languages', 'Optimization'],
        'dependency_replacements': {
            'x86_old': 'Modern x86-64'
        },
        'code_examples': {
            'old': "mov ax, 1\nint 21h",
            'new': "#include <stdio.h>\nint main() {\n    printf('Hello World');\n    return 0;\n}"
        },
        'risks': ['Performance trade-offs', 'Manual optimization loss', 'Portability issues']
    },
    'ASP': {
        'modern_frameworks': ['ASP.NET', 'Node.js with Express', 'Python with Django'],
        'patterns': ['MVC', 'Web APIs', 'Modern web standards'],
        'dependency_replacements': {
            'asp3': 'ASP.NET Core'
        },
        'code_examples': {
            'old': "<% Response.Write('Hello World') %>",
            'new': "<%@ Page Language='C#' %>\n<%= 'Hello World' %>"
        },
        'risks': ['Complete rewrite often needed', 'Database migration', 'Client-side changes']
    },
    'ColdFusion': {
        'modern_frameworks': ['Java with Spring', 'Node.js', '.NET'],
        'patterns': ['RESTful APIs', 'Microservices', 'Modern CFML'],
        'dependency_replacements': {
            'cf8': 'ColdFusion 2018+'
        },
        'code_examples': {
            'old': "<cfset message = 'Hello World'>\n<cfoutput>#message#</cfoutput>",
            'new': "<cfscript>\n    message = 'Hello World';\n    writeOutput(message);\n</cfscript>"
        },
        'risks': ['Tag-based to script migration', 'Server changes', 'Performance tuning']
    },
    'ActionScript': {
        'modern_frameworks': ['JavaScript/TypeScript', 'HTML5 Canvas', 'WebGL'],
        'patterns': ['ES6+ JavaScript', 'Frameworks like React', 'Web standards'],
        'dependency_replacements': {
            'as2': 'ActionScript 3 or JavaScript'
        },
        'code_examples': {
            'old': "trace('Hello World');",
            'new': "console.log('Hello World');"
        },
        'risks': ['Flash end-of-life', 'Browser compatibility', 'API changes']
    },
    'Objective-C': {
        'modern_frameworks': ['Swift', 'Modern Objective-C'],
        'patterns': ['Swift interoperability', 'ARC', 'Modern iOS development'],
        'dependency_replacements': {
            'objc1': 'Objective-C 2.0+'
        },
        'code_examples': {
            'old': "NSLog(@\"Hello World\");",
            'new': "NSLog(@\"Hello World\"); // or Swift: print(\"Hello World\")"
        },
        'risks': ['Syntax learning', 'Memory management changes', 'API evolution']
    },
    'Delphi': {
        'modern_frameworks': ['C# with .NET', 'Modern Delphi'],
        'patterns': ['Cross-platform with FireMonkey', 'Modern Pascal', 'RAD tools'],
        'dependency_replacements': {
            'delphi7': 'Delphi 10+'
        },
        'code_examples': {
            'old': "ShowMessage('Hello World');",
            'new': "ShowMessage('Hello World');"
        },
        'risks': ['Vendor lock-in', 'Cost of modern Delphi', 'Cross-platform considerations']
    },
    'RPG': {
        'modern_frameworks': ['Java', 'C#', '.NET'],
        'patterns': ['ILE RPG', 'Web services', 'Modernization'],
        'dependency_replacements': {
            'rpgiii': 'RPG IV'
        },
        'code_examples': {
            'old': "C                   'Hello World'  DSPLY",
            'new': "dsply 'Hello World';"
        },
        'risks': ['Mainframe dependency', 'Skill availability', 'Cost of migration']
    },
    'Lisp': {
        'modern_frameworks': ['Clojure', 'Common Lisp (modern)', 'Scheme'],
        'patterns': ['Functional programming', 'Macros', 'REPL-driven development'],
        'dependency_replacements': {
            'common_lisp_old': 'SBCL or Clojure'
        },
        'code_examples': {
            'old': "(defun hello () (print 'Hello World'))",
            'new': "(defn hello [] (println \"Hello World\"))"
        },
        'risks': ['Syntax differences', 'Library ecosystem changes', 'Performance considerations']
    },
    'Scheme': {
        'modern_frameworks': ['Racket', 'Guile', 'Chicken Scheme'],
        'patterns': ['Functional programming', 'Continuations', 'Macros'],
        'dependency_replacements': {
            'scheme_old': 'Racket'
        },
        'code_examples': {
            'old': "(define (hello) (display 'Hello World'))",
            'new': "(define (hello) (display \"Hello World\"))"
        },
        'risks': ['Dialect differences', 'Limited adoption', 'Learning curve']
    },
    'Prolog': {
        'modern_frameworks': ['SWI-Prolog', 'ECLiPSe', 'Python with logic libraries'],
        'patterns': ['Logic programming', 'Constraint solving', 'AI applications'],
        'dependency_replacements': {
            'prolog_old': 'SWI-Prolog'
        },
        'code_examples': {
            'old': "hello :- write('Hello World').",
            'new': "hello :- write('Hello World')."
        },
        'risks': ['Niche use cases', 'Performance for large datasets', 'Integration challenges']
    },
    'Smalltalk': {
        'modern_frameworks': ['Pharo', 'Squeak', 'Amber Smalltalk'],
        'patterns': ['Object-oriented programming', 'Live coding', 'Image-based development'],
        'dependency_replacements': {
            'smalltalk_old': 'Pharo'
        },
        'code_examples': {
            'old': "Transcript show: 'Hello World'",
            'new': "Transcript show: 'Hello World'"
        },
        'risks': ['Small community', 'Image-based paradigm', 'Tooling limitations']
    },
    'HyperTalk': {
        'modern_frameworks': ['JavaScript', 'HTML5', 'Web technologies'],
        'patterns': ['Event-driven programming', 'GUI development', 'Scripting'],
        'dependency_replacements': {
            'hypertalk': 'JavaScript'
        },
        'code_examples': {
            'old': "on mouseUp\n  put 'Hello World' into field 'output'\nend mouseUp",
            'new': "document.getElementById('output').innerText = 'Hello World';"
        },
        'risks': ['Complete paradigm shift', 'Browser compatibility', 'Security model changes']
    },
    'Tcl/Tk': {
        'modern_frameworks': ['Python with Tkinter', 'Ruby with Tk', 'Modern Tcl'],
        'patterns': ['Scripting', 'GUI development', 'Embeddable languages'],
        'dependency_replacements': {
            'tcl_old': 'Tcl 8.6+'
        },
        'code_examples': {
            'old': "puts 'Hello World'",
            'new': "puts 'Hello World'"
        },
        'risks': ['Syntax differences from mainstream languages', 'GUI toolkit changes', 'Community size']
    },
    'VB.NET': {
        'modern_frameworks': ['C#', '.NET Core', 'F#'],
        'patterns': ['Object-oriented programming', '.NET ecosystem', 'Cross-platform development'],
        'dependency_replacements': {
            'vb.net_old': '.NET 6+'
        },
        'code_examples': {
            'old': "MsgBox('Hello World')",
            'new': "MessageBox.Show('Hello World')"
        },
        'risks': ['Language syntax changes', 'Framework updates', 'IDE migration']
    },
    'FoxPro': {
        'modern_frameworks': ['C# with .NET', 'Python with SQLAlchemy', 'Java with Hibernate'],
        'patterns': ['Database applications', 'RAD tools', 'Modern ORMs'],
        'dependency_replacements': {
            'foxpro_old': 'Visual FoxPro or alternatives'
        },
        'code_examples': {
            'old': "USE customers\nBROWSE",
            'new': "using (var context = new DbContext()) { var customers = context.Customers.ToList(); }"
        },
        'risks': ['Database migration complexity', 'UI redesign', 'Skill transition']
    },
    'Logo': {
        'modern_frameworks': ['Python with Turtle', 'JavaScript Canvas', 'Scratch'],
        'patterns': ['Educational programming', 'Graphics', 'Turtle graphics'],
        'dependency_replacements': {
            'logo_old': 'Python Turtle'
        },
        'code_examples': {
            'old': "forward 100\nright 90",
            'new': "turtle.forward(100)\nturtle.right(90)"
        },
        'risks': ['Educational focus', 'Limited professional use', 'Syntax learning']
    },
    'Clipper': {
        'modern_frameworks': ['C#', 'Python', 'Java'],
        'patterns': ['Database applications', 'Business logic', 'Modern frameworks'],
        'dependency_replacements': {
            'clipper_old': 'Harbour or alternatives'
        },
        'code_examples': {
            'old': "FUNCTION Main()\n  ? 'Hello World'\nRETURN NIL",
            'new': "public static void Main() {\n    Console.WriteLine('Hello World');\n}"
        },
        'risks': ['Complete rewrite', 'Database handling changes', 'UI migration']
    },
    'BASIC': {
        'modern_frameworks': ['Python', 'C#', 'JavaScript'],
        'patterns': ['Structured programming', 'Object-oriented', 'Modern syntax'],
        'dependency_replacements': {
            'basic_old': 'Visual Basic .NET or Python'
        },
        'code_examples': {
            'old': "PRINT 'Hello World'",
            'new': "print('Hello World')"
        },
        'risks': ['Syntax modernization', 'Library changes', 'Performance differences']
    },
    'Modula-2': {
        'modern_frameworks': ['C', 'C++', 'Ada'],
        'patterns': ['Modular programming', 'Systems programming', 'Safety'],
        'dependency_replacements': {
            'modula2': 'C or C++'
        },
        'code_examples': {
            'old': "MODULE Hello;\n  PROCEDURE Main();\n  BEGIN\n    WriteString('Hello World');\n  END Main;\nEND Hello.",
            'new': "#include <stdio.h>\nint main() {\n    printf('Hello World');\n    return 0;\n}"
        },
        'risks': ['Manual translation', 'Memory management changes', 'Toolchain updates']
    },
    'D': {
        'modern_frameworks': ['D (modern)', 'C++', 'Rust'],
        'patterns': ['Systems programming', 'Performance', 'Safety'],
        'dependency_replacements': {
            'd1': 'D 2.x'
        },
        'code_examples': {
            'old': "import std.stdio;\nvoid main() {\n  writefln('Hello World');\n}",
            'new': "import std.stdio;\nvoid main() {\n  writeln('Hello World');\n}"
        },
        'risks': ['Version compatibility', 'Library changes', 'GC vs manual memory']
    },
    'REXX': {
        'modern_frameworks': ['Python', 'Perl', 'Ruby'],
        'patterns': ['Scripting', 'Text processing', 'System administration'],
        'dependency_replacements': {
            'rexx_old': 'Python or Perl'
        },
        'code_examples': {
            'old': "say 'Hello World'",
            'new': "print('Hello World')"
        },
        'risks': ['Syntax differences', 'Platform dependencies', 'Limited modern support']
    },
    'Erlang': {
        'modern_frameworks': ['Elixir', 'Erlang (modern)', 'Scala'],
        'patterns': ['Concurrent programming', 'Fault tolerance', 'Distributed systems'],
        'dependency_replacements': {
            'erlang_old': 'Erlang 24+ or Elixir'
        },
        'code_examples': {
            'old': "-module(hello).\n-export([start/0]).\nstart() -> io:format('Hello World~n').",
            'new': "defmodule Hello do\n  def start do\n    IO.puts('Hello World')\n  end\nend"
        },
        'risks': ['Syntax changes', 'OTP updates', 'Ecosystem differences']
    }
}

def transform_code(content, lang):
    if lang == 'PHP':
        # Replace mysql_ functions with PDO equivalents
        content = re.sub(r'mysql_query\(([^)]+)\)', r'$pdo->query(\1)', content)
        content = re.sub(r'mysql_fetch_array\(([^)]+)\)', r'$stmt->fetch(\1)', content)
        content = re.sub(r'mysql_num_rows\(([^)]+)\)', r'$stmt->rowCount()', content)
        content = re.sub(r'mysql_connect\(([^,]+),\s*([^,]+),\s*([^)]+)\)', r'new PDO("mysql:host=\1;dbname=", \2, \3)', content)
        # Note: mysql_select_db needs manual handling, as dbname should be in DSN
    elif lang == 'VB':
        # Basic conversions to VB.NET
        content = re.sub(r'MsgBox\s*\(', 'MessageBox.Show(', content)
        content = re.sub(r'MsgBox\s+([^,\n]+)', r'MessageBox.Show(\1)', content)
    return content

def write_migrated_file(content, lang, migrated_filepath):
    transformed_content = transform_code(content, lang)
    with open(migrated_filepath, 'w', encoding='utf-8') as mf:
        mf.write(transformed_content)

def generate_migration_suggestions(analysis_results, business_logic, target, path=None):
    """
    Generate intelligent migration proposals based on analysis results.
    """
    suggestions = {}
    languages_found = set()

    # Collect languages from dependencies
    for dep in analysis_results.get('dependencies', []):
        languages_found.add(dep['language'])

    # Also detect languages from files if path is provided
    if path:
        for root, dirs, files in os.walk(path):
            for file in files:
                ext = os.path.splitext(file)[1].lower()
                if ext in ['.php', '.vb', '.cbl', '.java', '.sh', '.bat', '.pl', '.f', '.f90', '.ada', '.pas', '.asm', '.asp', '.cfm', '.as', '.m', '.dpr', '.rpg', '.lisp', '.lsp', '.scm', '.st', '.ht', '.tcl', '.prg', '.fox', '.lgo', '.bas', '.mod', '.m2', '.d', '.rex', '.cmd', '.erl']:
                    filepath = os.path.join(root, file)
                    try:
                        with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
                            content = f.read()
                            lang = detect_language_from_content(content, ext)
                            if lang:
                                languages_found.add(lang)
                    except Exception as e:
                        logging.warning(f"Error detecting language in {filepath}: {e}")

    for lang in languages_found:
        if lang in MIGRATION_SUGGESTIONS:
            sug = MIGRATION_SUGGESTIONS[lang]
            suggestions[lang] = {
                'recommended_frameworks': sug['modern_frameworks'],
                'architectural_patterns': sug['patterns'],
                'dependency_updates': sug['dependency_replacements'],
                'code_transformation_example': sug['code_examples'],
                'risk_assessment': sug['risks'],
                'business_logic_preservation': f"Extracted {len(business_logic.get('functions', []))} functions, {len(business_logic.get('classes', []))} classes, {len(business_logic.get('variables', []))} variables, and {len(business_logic.get('control_flows', []))} control flows. Ensure these are mapped to new framework structures."
            }

    # If target specified, prioritize suggestions towards target
    if target and target != 'auto':
        # Filter or adjust suggestions based on target
        for lang in list(suggestions.keys()):
            if target.lower() in [f.lower() for f in suggestions[lang]['recommended_frameworks']]:
                # Prioritize this
                pass

    return suggestions

def migrate_code(path, target, write=False):
    """
    Analyze code and provide migration suggestions.
    If write is True, write migrated files to disk.
    """
    # Analyze the codebase
    analysis = analyze_code(path)

    # Extract business logic (simplified: assume one main file or aggregate)
    business_logic = {}
    for root, dirs, files in os.walk(path):
        for file in files:
            ext = os.path.splitext(file)[1].lower()
            if ext in ['.php', '.vb', '.cbl', '.java', '.sh', '.bat', '.pl', '.f', '.f90', '.ada', '.pas', '.asm', '.asp', '.cfm', '.as', '.m', '.dpr', '.rpg', '.lisp', '.lsp', '.scm', '.st', '.ht', '.tcl', '.prg', '.fox', '.lgo', '.bas', '.mod', '.m2', '.d', '.rex', '.cmd', '.erl']:
                filepath = os.path.join(root, file)
                try:
                    with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
                        content = f.read()
                        lang = detect_language_from_content(content, ext)
                        if lang:
                            bl = extract_business_logic(content, lang)
                            # Aggregate
                            for key in bl:
                                if isinstance(bl[key], list):
                                    business_logic.setdefault(key, []).extend(bl[key])
                                else:
                                    business_logic[key] = bl[key]
                except Exception as e:
                    logging.warning(f"Error extracting business logic from {filepath}: {e}")

    # Generate suggestions
    suggestions = generate_migration_suggestions(analysis, business_logic, target, path)

    # Format output
    if write:
        # Create migrated directory
        migrated_dir = os.path.join(os.getcwd(), 'migrated')
        os.makedirs(migrated_dir, exist_ok=True)
        output = f"Migration Writing for {path}\n"
        output += f"Total files: {analysis['total_files']}, Total lines: {analysis['total_lines']}\n\n"
        output += f"Writing migrated files to {migrated_dir}\n\n"
        # Process files
        files_migrated = 0
        for root, dirs, files in os.walk(path):
            for file in files:
                ext = os.path.splitext(file)[1].lower()
                if ext in ['.php', '.vb', '.cbl', '.java', '.sh', '.bat', '.pl', '.f', '.f90', '.ada', '.pas', '.asm', '.asp', '.cfm', '.as', '.m', '.dpr', '.rpg', '.lisp', '.lsp', '.scm', '.st', '.ht', '.tcl', '.prg', '.fox', '.lgo', '.bas', '.mod', '.m2', '.d', '.rex', '.cmd', '.erl']:
                    filepath = os.path.join(root, file)
                    try:
                        with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
                            content = f.read()
                            lang = detect_language_from_content(content, ext)
                            if lang:
                                migrated_filepath = os.path.join(migrated_dir, file)
                                write_migrated_file(content, lang, migrated_filepath)
                                files_migrated += 1
                                output += f"Migrated: {filepath} -> {migrated_filepath}\n"
                    except Exception as e:
                        logging.warning(f"Error migrating file {filepath}: {e}")
        output += f"\nTotal files migrated: {files_migrated}\n"
        output += "\nMigration Suggestions:\n"
        for lang, sug in suggestions.items():
            output += f"Language: {lang}\n"
            output += f"Recommended Frameworks: {', '.join(sug['recommended_frameworks'])}\n"
            output += f"Architectural Patterns: {', '.join(sug['architectural_patterns'])}\n"
            output += f"Dependency Updates: {sug['dependency_updates']}\n"
            output += f"Code Example - Old: {sug['code_transformation_example']['old']}\n"
            output += f"Code Example - New: {sug['code_transformation_example']['new']}\n"
            output += f"Risks: {', '.join(sug['risk_assessment'])}\n"
            output += f"Business Logic: {sug['business_logic_preservation']}\n\n"
    else:
        output = f"Migration Analysis for {path}\n"
        output += f"Total files: {analysis['total_files']}, Total lines: {analysis['total_lines']}\n\n"
        for lang, sug in suggestions.items():
            output += f"Language: {lang}\n"
            output += f"Recommended Frameworks: {', '.join(sug['recommended_frameworks'])}\n"
            output += f"Architectural Patterns: {', '.join(sug['architectural_patterns'])}\n"
            output += f"Dependency Updates: {sug['dependency_updates']}\n"
            output += f"Code Example - Old: {sug['code_transformation_example']['old']}\n"
            output += f"Code Example - New: {sug['code_transformation_example']['new']}\n"
            output += f"Risks: {', '.join(sug['risk_assessment'])}\n"
            output += f"Business Logic: {sug['business_logic_preservation']}\n\n"

    return output