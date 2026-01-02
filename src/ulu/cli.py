import click
import shlex
import os
from . import __version__
from .detector import detect_language
from .analyzer import analyze_code
from .migrator import migrate_code
from .reporter import generate_report
from .validator import validate_project
from .comparator import generate_diff_report
from .security import scan_secrets, scan_vulnerabilities, generate_security_report
from .cleaner import cleanup_project
from .packer import package_project
from .compatibility import analyze_compatibility
from .config import get_config, set_config, list_config
from .update import check_for_updates
from .init import initialize_project

LOGO = r"""
  ___    _   .---.      ___    _
 .'   |  | |  | ,_|    .'   |  | |
 |   .'  | |,-./  )    |   .'  | |
 .'  '_  | |\  '_ '`)  .'  '_  | |
 '   ( \.-.| > (_)  )  '   ( \.-.|
 ' (`. _` /|(  .  .-'  ' (`. _` /|
 | (_ (_) _) `-'`-'|___| (_ (_) _)
  \ /  . \ /  |        \\ /  . \ /
   ``-'`-''   `--------` ``-'`-''
 """

@click.group()
def cli():
    """Universal Legacy Updater CLI"""
    pass

@cli.command()
@click.argument('path', type=click.Path(exists=True))
def detect(path):
    """Detect legacy code in the given path"""
    print(LOGO)
    languages = detect_language(path)
    click.echo(f"Detected languages: {languages}")

@cli.command()
@click.argument('path', type=click.Path(exists=True))
def analyze(path):
    """Analyze codebase"""
    print(LOGO)
    analysis = analyze_code(path)
    click.echo(f"Analysis: {analysis}")

@cli.command()
@click.argument('path', type=click.Path(exists=True))
@click.option('--target', default='auto', help='Target language')
@click.option('--write', is_flag=True, help='Write migrated files to disk')
def migrate(path, target, write):
    """Migrate legacy code"""
    print(LOGO)
    result = migrate_code(path, target, write)
    click.echo(result)

@cli.command()
@click.argument('path', type=click.Path(exists=True))
@click.option('--target', default='auto', help='Target language')
def simulate(path, target):
    """Simulate migration without writing files (alias for migrate without --write)"""
    print(LOGO)
    result = migrate_code(path, target, write=False)
    click.echo(result)

@cli.command()
@click.argument('path', type=click.Path(exists=True))
@click.option('--target', default='auto', help='Target language')
@click.option('--output', default=None, help='Output Markdown file path')
def report(path, target, output):
    """Generate comprehensive migration report"""
    print(LOGO)
    from .migrator import generate_migration_suggestions
    from .analyzer import analyze_code, extract_business_logic, detect_language_from_content
    import os

    # Analyze
    analysis_results = analyze_code(path)

    # Extract business logic
    business_logic = {}
    for root, dirs, files in os.walk(path):
        for file in files:
            ext = os.path.splitext(file)[1].lower()
            if ext in ['.php', '.vb', '.cbl', '.java', '.sh', '.bat', '.pl']:
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
                except:
                    pass

    # Generate suggestions
    migration_suggestions = generate_migration_suggestions(analysis_results, business_logic, target, path)

    # Generate report
    report_md = generate_report(analysis_results, migration_suggestions, business_logic, output)
    # generate_report already prints to console and saves to file if output provided


@cli.command()
@click.argument('path', type=click.Path(exists=True))
def validate(path):
    """Validate migrated project"""
    print(LOGO)
    result = validate_project(path)
    click.echo(result)

@cli.command()
@click.argument('legacy_path', type=click.Path(exists=True))
@click.argument('modernized_path', type=click.Path(exists=True))
def compare(legacy_path, modernized_path):
    """Compare legacy and modernized code"""
    print(LOGO)
    report = generate_diff_report(legacy_path, modernized_path)
    click.echo(report)

@cli.command()
@click.argument('path', type=click.Path(exists=True))
def security(path):
    """Perform comprehensive security analysis including secrets detection"""
    print(LOGO)
    secrets = scan_secrets(path)
    vulnerabilities = scan_vulnerabilities(path)
    report = generate_security_report(secrets, vulnerabilities, path)
    click.echo(report)

@cli.command()
@click.argument('path', type=click.Path(exists=True))
def cleanup(path):
    """Cleanup and normalize the migrated project"""
    print(LOGO)
    result = cleanup_project(path)
    click.echo(result)

@cli.command()
@click.argument('path', type=click.Path(exists=True))
@click.option('--format', type=click.Choice(['zip', 'tar', 'docker']), default='zip', help='Packaging format')
@click.option('--output', required=True, help='Output path')
def package(path, format, output):
    """Package migrated project"""
    print(LOGO)
    result = package_project(path, format, output)
    click.echo(result)

@cli.command()
@click.argument('path', type=click.Path(exists=True))
@click.option('--targets', default='windows,linux,macos,python3.8,python3.9,python3.10', help='Comma-separated list of target environments')
def compatibility(path, targets):
    """Analyze compatibility of migrated project for different target environments"""
    print(LOGO)
    target_list = [t.strip() for t in targets.split(',')]
    results = analyze_compatibility(path, target_list)

    for env, data in results.items():
        click.echo(f"\n{env.upper()}:")
        click.echo(f"  Status: {data['status']}")
        if 'issues' in data and data['issues']:
            click.echo("  Issues:")
            for issue in data['issues']:
                click.echo(f"    - {issue}")
        if 'min_required' in data:
            click.echo(f"  Minimum Python version required: {data['min_required']}")
        if 'conflicts' in data and data['conflicts']:
            click.echo("  Conflicts:")
            for conflict in data['conflicts']:
                click.echo(f"    - {conflict}")
        if 'missing' in data and data['missing']:
            click.echo("  Missing dependencies:")
            for miss in data['missing']:
                click.echo(f"    - {miss}")

@cli.command()
def version():
    """Show tool version"""
    print(LOGO)
    click.echo(f"Universal Legacy Updater version {__version__}")

@cli.command()
@click.option('--path', default='.', type=click.Path(), help='Path to initialize the project')
def init(path):
    """Initialize migration project structure"""
    print(LOGO)
    initialize_project(path)

@cli.group()
def config():
    """Manage configuration settings"""
    pass

@config.command()
@click.argument('section')
@click.argument('key')
def get(section, key):
    value = get_config(section, key)
    if value is None:
        click.echo(f"No value found for {section}.{key}")
    else:
        click.echo(value)

@config.command()
@click.argument('section')
@click.argument('key')
@click.argument('value')
def set(section, key, value):
    set_config(section, key, value)
    click.echo(f"Set {section}.{key} = {value}")

@config.command()
def list():
    output = list_config()
    click.echo(output)

@cli.command()
def update():
    """Check for tool updates"""
    print(LOGO)
    result = check_for_updates()
    click.echo(result)


def interactive():
    print(LOGO)
    while True:
        try:
            cmd_line = input('ULU> ').strip()
            if not cmd_line:
                continue
            if cmd_line.lower() in ['exit', 'quit']:
                break
            parts = shlex.split(cmd_line)
            if not parts:
                continue
            command = parts[0].lower()
            args = parts[1:]
            if command == 'detect':
                if len(args) != 1:
                    print("Usage: detect <path>")
                    continue
                path = args[0]
                if not os.path.exists(path):
                    print(f"Path does not exist: {path}")
                    continue
                languages = detect_language(path)
                print(f"Detected languages: {languages}")
            elif command == 'analyze':
                if len(args) != 1:
                    print("Usage: analyze <path>")
                    continue
                path = args[0]
                if not os.path.exists(path):
                    print(f"Path does not exist: {path}")
                    continue
                analysis = analyze_code(path)
                print(f"Analysis: {analysis}")
            elif command == 'migrate':
                if not args:
                    print("Usage: migrate <path> [--target TARGET] [--write]")
                    continue
                path = args[0]
                if not os.path.exists(path):
                    print(f"Path does not exist: {path}")
                    continue
                target = 'auto'
                write = False
                i = 1
                while i < len(args):
                    if args[i] == '--target' and i + 1 < len(args):
                        target = args[i + 1]
                        i += 2
                    elif args[i] == '--write':
                        write = True
                        i += 1
                    else:
                        print("Usage: migrate <path> [--target TARGET] [--write]")
                        i = len(args)  # break
                if i == len(args):
                    result = migrate_code(path, target, write)
                    print(result)
            elif command == 'report':
                if not args:
                    print("Usage: report <path> [--target TARGET] [--output OUTPUT]")
                    continue
                path = args[0]
                if not os.path.exists(path):
                    print(f"Path does not exist: {path}")
                    continue
                target = 'auto'
                output = None
                i = 1
                while i < len(args):
                    if args[i] == '--target' and i + 1 < len(args):
                        target = args[i + 1]
                        i += 2
                    elif args[i] == '--output' and i + 1 < len(args):
                        output = args[i + 1]
                        i += 2
                    else:
                        print("Usage: report <path> [--target TARGET] [--output OUTPUT]")
                        i = len(args)  # break
                if i == len(args):  # parsed successfully
                    from .migrator import generate_migration_suggestions
                    from .analyzer import analyze_code, extract_business_logic, detect_language_from_content
                    # Analyze
                    analysis_results = analyze_code(path)
                    # Extract business logic
                    business_logic = {}
                    for root, dirs, files in os.walk(path):
                        for file in files:
                            ext = os.path.splitext(file)[1].lower()
                            if ext in ['.php', '.vb', '.cbl', '.java', '.sh', '.bat', '.pl']:
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
                                except:
                                    pass
                    # Generate suggestions
                    migration_suggestions = generate_migration_suggestions(analysis_results, business_logic, target, path)
                    # Generate report
                    report_md = generate_report(analysis_results, migration_suggestions, business_logic, output)
            elif command == 'validate':
                if len(args) != 1:
                    print("Usage: validate <path>")
                    continue
                path = args[0]
                if not os.path.exists(path):
                    print(f"Path does not exist: {path}")
                    continue
                result = validate_project(path)
                print(result)
            elif command == 'compare':
                if len(args) != 2:
                    print("Usage: compare <legacy_path> <modernized_path>")
                    continue
                legacy_path, modernized_path = args
                if not os.path.exists(legacy_path) or not os.path.exists(modernized_path):
                    print("Paths do not exist")
                    continue
                report = generate_diff_report(legacy_path, modernized_path)
                print(report)
            elif command == 'security':
                if len(args) != 1:
                    print("Usage: security <path>")
                    continue
                path = args[0]
                if not os.path.exists(path):
                    print(f"Path does not exist: {path}")
                    continue
                secrets = scan_secrets(path)
                vulnerabilities = scan_vulnerabilities(path)
                report = generate_security_report(secrets, vulnerabilities, path)
                print(report)
            elif command == 'cleanup':
                if len(args) != 1:
                    print("Usage: cleanup <path>")
                    continue
                path = args[0]
                if not os.path.exists(path):
                    print(f"Path does not exist: {path}")
                    continue
                result = cleanup_project(path)
                print(result)
            elif command == 'package':
                if not args:
                    print("Usage: package <path> --format FORMAT --output OUTPUT")
                    continue
                path = args[0]
                if not os.path.exists(path):
                    print(f"Path does not exist: {path}")
                    continue
                format_type = 'zip'
                output = None
                i = 1
                while i < len(args):
                    if args[i] == '--format' and i + 1 < len(args):
                        format_type = args[i + 1]
                        if format_type not in ['zip', 'tar', 'docker']:
                            print("Invalid format. Choose from zip, tar, docker")
                            i = len(args)
                            continue
                        i += 2
                    elif args[i] == '--output' and i + 1 < len(args):
                        output = args[i + 1]
                        i += 2
                    else:
                        print("Usage: package <path> --format FORMAT --output OUTPUT")
                        i = len(args)
                if i == len(args) and output:
                    result = package_project(path, format_type, output)
                    print(result)
            elif command == 'compatibility':
                if not args:
                    print("Usage: compatibility <path> [--targets TARGETS]")
                    continue
                path = args[0]
                if not os.path.exists(path):
                    print(f"Path does not exist: {path}")
                    continue
                targets = 'windows,linux,macos,python3.8,python3.9,python3.10'
                i = 1
                while i < len(args):
                    if args[i] == '--targets' and i + 1 < len(args):
                        targets = args[i + 1]
                        i += 2
                    else:
                        print("Usage: compatibility <path> [--targets TARGETS]")
                        i = len(args)
                if i == len(args):
                    target_list = [t.strip() for t in targets.split(',')]
                    results = analyze_compatibility(path, target_list)
                    for env, data in results.items():
                        print(f"\n{env.upper()}:")
                        print(f"  Status: {data['status']}")
                        if 'issues' in data and data['issues']:
                            print("  Issues:")
                            for issue in data['issues']:
                                print(f"    - {issue}")
                        if 'min_required' in data:
                            print(f"  Minimum Python version required: {data['min_required']}")
                        if 'conflicts' in data and data['conflicts']:
                            print("  Conflicts:")
                            for conflict in data['conflicts']:
                                print(f"    - {conflict}")
                        if 'missing' in data and data['missing']:
                            print("  Missing dependencies:")
                            for miss in data['missing']:
                                print(f"    - {miss}")
            elif command == 'simulate':
                if not args:
                    print("Usage: simulate <path> [--target TARGET]")
                    continue
                path = args[0]
                if not os.path.exists(path):
                    print(f"Path does not exist: {path}")
                    continue
                target = 'auto'
                i = 1
                while i < len(args):
                    if args[i] == '--target' and i + 1 < len(args):
                        target = args[i + 1]
                        i += 2
                    else:
                        print("Usage: simulate <path> [--target TARGET]")
                        i = len(args)
                if i == len(args):
                    result = migrate_code(path, target, dry_run=True)
                    print(result)
            elif command == 'version':
                print(f"Universal Legacy Updater version {__version__}")
            elif command == 'init':
                path = args[0] if args else '.'
                initialize_project(path)
            elif command == 'config':
                if len(args) < 1:
                    print("Usage: config <get|set|list> [args]")
                    continue
                subcmd = args[0]
                if subcmd == 'get' and len(args) == 3:
                    section, key = args[1], args[2]
                    value = get_config(section, key)
                    if value is None:
                        print(f"No value found for {section}.{key}")
                    else:
                        print(value)
                elif subcmd == 'set' and len(args) == 4:
                    section, key, value = args[1], args[2], args[3]
                    set_config(section, key, value)
                    print(f"Set {section}.{key} = {value}")
                elif subcmd == 'list':
                    output = list_config()
                    print(output)
                else:
                    print("Usage: config get <section> <key> | config set <section> <key> <value> | config list")
            elif command == 'update':
                result = check_for_updates()
                print(result)
            else:
                print("Unknown command. Available: detect, analyze, migrate, simulate, report, validate, compare, security, cleanup, package, compatibility, version, init, config, update, exit, quit")
        except KeyboardInterrupt:
            print("\nExiting...")
            break
        except EOFError:
            print("\nExiting...")
            break
        except Exception as e:
            print(f"Error: {e}")