import os

def generate_report(analysis_results, migration_suggestions, business_logic, output_file=None):
    """
    Generate a comprehensive report aggregating analysis and migration results.

    Args:
        analysis_results (dict): Results from code analysis containing total_files, total_lines, dependencies, vulnerabilities, bad_practices.
        migration_suggestions (dict): Migration suggestions per language.
        business_logic (dict): Extracted business logic summary.
        output_file (str, optional): Path to output Markdown file. If None, only console output.

    Returns:
        str: The report in Markdown format.
    """
    report = []

    # Title
    report.append("# Universal Legacy Updater Migration Report\n")

    # Overview
    total_files = analysis_results.get('total_files', 0)
    total_lines = analysis_results.get('total_lines', 0)
    report.append("## Overview\n")
    report.append(f"- **Total Files Analyzed**: {total_files}")
    report.append(f"- **Total Lines of Code**: {total_lines}\n")

    # Detected Languages/Versions
    languages = set()
    versions = {}
    for dep in analysis_results.get('dependencies', []):
        lang = dep['language']
        languages.add(lang)
        if dep['status'] == 'obsolete':
            versions.setdefault(lang, []).append(dep['dependency'])
    report.append("## Detected Languages/Versions\n")
    if languages:
        report.append("**Languages**: " + ", ".join(sorted(languages)))
    else:
        report.append("No languages detected.")
    if versions:
        report.append("\n**Obsolete Versions/Dependencies**:")
        for lang, deps in versions.items():
            report.append(f"- {lang}: {', '.join(set(deps))}")
    report.append("")

    # Dependencies Status
    deps = analysis_results.get('dependencies', [])
    dep_status = {}
    for dep in deps:
        status = dep['status']
        dep_status.setdefault(status, []).append(f"{dep['dependency']} ({dep['file']})")
    report.append("## Dependencies Status\n")
    if dep_status:
        for status, items in dep_status.items():
            report.append(f"### {status.capitalize()}")
            for item in items:
                report.append(f"- {item}")
            report.append("")
    else:
        report.append("No dependencies found.\n")

    # Security Vulnerabilities
    vulns = analysis_results.get('vulnerabilities', [])
    report.append("## Security Vulnerabilities\n")
    if vulns:
        for vuln in vulns:
            report.append(f"- **{vuln['severity'].upper()}**: {vuln['description']} at {vuln['location']}")
    else:
        report.append("No vulnerabilities detected.\n")

    # Bad Practices
    bad_practs = analysis_results.get('bad_practices', [])
    report.append("## Bad Practices\n")
    if bad_practs:
        for bp in bad_practs:
            report.append(f"- **{bp['severity'].upper()}**: {bp['description']} at {bp['location']}")
    else:
        report.append("No bad practices detected.\n")

    # Business Logic Summary
    report.append("## Business Logic Summary\n")
    funcs = business_logic.get('functions', [])
    classes = business_logic.get('classes', [])
    vars_ = business_logic.get('variables', [])
    cfs = business_logic.get('control_flows', [])
    pseudocode = business_logic.get('pseudocode', '')
    report.append(f"- **Functions/Procedures**: {len(funcs)}")
    report.append(f"- **Classes**: {len(classes)}")
    report.append(f"- **Variables**: {len(vars_)}")
    report.append(f"- **Control Flows**: {len(cfs)}")
    if pseudocode:
        report.append("\n**Pseudocode Overview**:\n```")
        report.append(pseudocode)
        report.append("```\n")
    else:
        report.append("")

    # Migration Suggestions
    report.append("## Migration Suggestions\n")
    if migration_suggestions:
        for lang, sug in migration_suggestions.items():
            report.append(f"### {lang}")
            frameworks = sug.get('recommended_frameworks', [])
            patterns = sug.get('architectural_patterns', [])
            deps_up = sug.get('dependency_updates', {})
            code_ex = sug.get('code_transformation_example', {})
            risks = sug.get('risk_assessment', [])
            bl_pres = sug.get('business_logic_preservation', '')
            if frameworks:
                report.append("**Recommended Frameworks**: " + ", ".join(frameworks))
            if patterns:
                report.append("**Architectural Patterns**: " + ", ".join(patterns))
            if deps_up:
                report.append("**Dependency Updates**:")
                for old, new in deps_up.items():
                    report.append(f"  - {old} -> {new}")
            if code_ex:
                old_code = code_ex.get('old', '')
                new_code = code_ex.get('new', '')
                if old_code:
                    report.append("**Code Example - Legacy**:\n```")
                    report.append(old_code)
                    report.append("```")
                if new_code:
                    report.append("**Code Example - Modern**:\n```")
                    report.append(new_code)
                    report.append("```")
            if risks:
                report.append("**Risks**: " + ", ".join(risks))
            if bl_pres:
                report.append(f"**Business Logic Preservation**: {bl_pres}")
            report.append("")
    else:
        report.append("No migration suggestions available.\n")

    # Risks
    report.append("## Risks\n")
    all_risks = []
    for sug in migration_suggestions.values():
        all_risks.extend(sug.get('risk_assessment', []))
    if all_risks:
        for risk in set(all_risks):
            report.append(f"- {risk}")
    else:
        report.append("No specific risks identified.\n")

    # Potential Improvements
    report.append("## Potential Improvements\n")
    report.append("- Implement automated testing to ensure migration correctness.")
    report.append("- Use version control for incremental migration.")
    report.append("- Consider containerization for easier deployment.")
    report.append("- Review and update security practices post-migration.")
    report.append("- Optimize performance in the new framework.\n")

    markdown_report = "\n".join(report)

    # Console output
    print(markdown_report)

    # File output
    if output_file:
        try:
            with open(output_file, 'w', encoding='utf-8') as f:
                f.write(markdown_report)
            print(f"\nReport saved to {output_file}")
        except Exception as e:
            print(f"Error saving report to file: {e}")

    return markdown_report