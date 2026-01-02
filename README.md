# UniversalLegacyUpdater

[![Python Version](https://img.shields.io/badge/python-3.9+-blue.svg)](https://www.python.org/downloads/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

```
  ___    _   .---.      ___    _
 .'   |  | |  | ,_|    .'   |  | |
 |   .'  | |,-./  )    |   .'  | |
 .'  '_  | |\  '_ '`)  .'  '_  | |
 '   ( \.-.| > (_)  )  '   ( \.-.|
 ' (`. _` /|(  .  .-'  ' (`. _` /|
 | (_ (_) _) `-'`-'|___| (_ (_) _)
  \ /  . \ /  |        \\ /  . \ /
   ``-'`-''   `--------` ``-'`-''
```

## Description

UniversalLegacyUpdater is a tool designed for automated migration of legacy codebases to modern versions while preserving 100% of the business logic. It supports over 30 legacy languages (see Supported Languages section below). Migration targets include:

- PHP 5 → PHP 8
- VB → C#
- COBOL → Java
- Old Java → Latest Java
- Obsolete scripts → Python or Node.js

The system consists of a CLI interface, detection mechanisms, analysis components, migration logic, and a reporting system.

## Supported Languages

The system supports over 30 legacy languages, including:

- ActionScript
- Ada
- Assembly (x86)
- BASIC (legacy)
- Clipper
- COBOL
- ColdFusion Markup
- D (legacy)
- Delphi
- Erlang (legacy)
- Fortran
- FoxPro
- HyperTalk
- Java (older versions)
- Lisp
- Logo
- Modula-2
- Objective-C
- Pascal
- Perl
- PHP 5
- Prolog
- REXX
- RPG (IBM i / AS400)
- Scheme
- Obsolete scripts (BAT, etc.)
- Smalltalk
- Tcl/Tk
- VB (Visual Basic)
- VB.NET (legacy)

## Features

- **Automatic Detection**: Identifies legacy codebases and languages.
- **In-depth Analysis**: Parses code, extracts business logic, dependencies, and structures.
- **Intelligent Migration**: Applies transformation rules to generate modern code.
- **Comprehensive Reporting**: Generates reports on migration success, issues, and recommendations.
- **Business Logic Preservation**: Ensures 100% of business logic is preserved during migration.
- **Multi-language Support**: Supports multiple legacy languages and modern targets.

## Installation

### Using the Installer Script (Recommended)

1. Download or clone the repository to your local machine.
2. Navigate to the project directory.
3. Run the installer script:

   ```cmd
   install.bat
   ```

   This script will:
   - Check for Python 3.x installation
   - Install required dependencies
   - Add the tool to your system PATH
   - Create a desktop shortcut for easy access
   - Verify the installation

4. If prompted, restart your command prompt for PATH changes to take effect.

### Manual Installation (Alternative)

If you prefer manual installation:

1. Ensure Python 3.6+ is installed.
2. Install dependencies:

   ```bash
   pip install -r requirements.txt
   ```

3. Add the project directory to your PATH or use full paths when running commands.

### Verification

After installation, verify by running:

```cmd
ulu version
```

You should see the version information.

## Building the Installer

To create a distributable .exe installer using NSIS:

1. Install NSIS (Nullsoft Scriptable Install System) from the official website: https://nsis.sourceforge.io/

2. Open a command prompt in the project directory.

3. Run the following command:

   ```cmd
   makensis setup.nsi
   ```

   This will generate `UniversalLegacyUpdater_Setup.exe` in the project directory.

## Distribution and Installation

### For End Users

The .exe installer provides a user-friendly way to install UniversalLegacyUpdater without requiring technical knowledge of Python or dependencies.

#### Installation Steps

1. Download `UniversalLegacyUpdater_Setup.exe` from the project's releases page or your distribution channel.

2. Run the installer executable as an administrator (right-click > Run as administrator).

3. Follow the installation wizard:
   - Welcome screen: Click Next
   - Choose installation directory: Default is recommended, or select a custom location
   - Installation progress: Wait for completion
   - Finish: Click Finish to complete installation

4. The installer automatically:
   - Verifies Python 3.9+ is installed
   - Installs required Python dependencies
   - Adds the tool to your system PATH
   - Creates desktop and Start Menu shortcuts
   - Registers the application for uninstallation

5. After installation, restart any open command prompts for PATH changes to take effect.

6. Verify installation by opening a new command prompt and running:

   ```cmd
   ulu version
   ```

   You should see the version information displayed.

#### System Requirements

- Windows 7 or later
- Python 3.9 or higher (installer will check and prompt if missing)
- Administrator privileges for installation
- Internet connection for dependency installation

#### Uninstalling

To uninstall UniversalLegacyUpdater:

1. Go to Windows Settings > Apps > Apps & features
2. Find "UniversalLegacyUpdater" in the list
3. Click Uninstall and follow the prompts

Alternatively, use the uninstaller in the Start Menu or desktop shortcut.

This section supersedes any conflicting general installation instructions.

### Usage

Once installed, you can use the tool from any command prompt:

```cmd
ulu <command> [options]
```

For example:

```cmd
ulu detect samples/
```

See the Usage section below for detailed commands.

## Usage

### Direct CLI Mode

The direct mode uses CLI commands for specific operations.

#### Available Commands

- `python src/main.py detect <path>`: Detect legacy code in the given path.
- `python src/main.py analyze <path>`: Analyze the codebase.
- `python src/main.py migrate <path> --target <target>`: Migrate legacy code (default target: 'auto').
- `python src/main.py simulate <path> --target <target>`: Simulate migration without writing files.
- `python src/main.py report <path> --target <target> --output <file>`: Generate comprehensive migration report.
- `python src/main.py validate <path>`: Validate the migrated project.
- `python src/main.py compare <legacy_path> <modernized_path>`: Compare legacy and modernized code.
- `python src/main.py security <path>`: Perform comprehensive security analysis including secrets detection.
- `python src/main.py cleanup <path>`: Clean up and normalize the migrated project.
- `python src/main.py package <path> --format <format> --output <file>`: Package the migrated project.
- `python src/main.py compatibility <path> --targets <targets>`: Analyze compatibility of migrated project for different target environments.
- `python src/main.py version`: Show tool version.
- `python src/main.py init --path <path>`: Initialize migration project structure.
- `python src/main.py config get <section> <key>`: Get a configuration value.
- `python src/main.py config set <section> <key> <value>`: Set a configuration value.
- `python src/main.py config list`: List all configurations.
- `python src/main.py update`: Check for tool updates.

#### Examples

Detect languages in the samples directory:

```bash
python src/main.py detect samples/
```

Analyze the codebase:

```bash
python src/main.py analyze samples/
```

Migrate legacy PHP code to PHP 8:

```bash
python src/main.py migrate samples/legacy_php.php --target php8
```

Generate a migration report:

```bash
python src/main.py report samples/ --target auto --output migration_report.md
```

Validate the migrated project:

```bash
python src/main.py validate samples/
```

Perform security analysis:

```bash
python src/main.py security samples/
```

Clean up the migrated project:

```bash
python src/main.py cleanup samples/
```

Package the migrated project:

```bash
python src/main.py package samples/ --format zip --output migrated_project.zip
```

Analyze compatibility:

```bash
python src/main.py compatibility samples/ --targets windows,linux,python3.9
```

Show version:

```bash
python src/main.py version
```

Initialize a migration project:

```bash
python src/main.py init --path ./new_project
```

Manage configuration:

```bash
python src/main.py config set migration default_target php8
python src/main.py config get migration default_target
python src/main.py config list
```

Check for updates:

```bash
python src/main.py update
```

### Interactive Mode

The interactive mode provides a shell for entering commands repeatedly without relaunching the program.

To launch interactive mode:

```bash
python -c "from src.ulu.cli import interactive; interactive()"
```

In the interactive shell, enter commands like:

- `detect <path>`
- `analyze <path>`
- `migrate <path> --target <target>`
- `simulate <path> --target <target>`
- `report <path> --target <target> --output <file>`
- `validate <path>`
- `compare <legacy_path> <modernized_path>`
- `security <path>`
- `cleanup <path>`
- `package <path> --format <format> --output <file>`
- `compatibility <path> --targets <targets>`
- `version`
- `init --path <path>`
- `config get <section> <key>`
- `config set <section> <key> <value>`
- `config list`
- `update`

Type `exit` or `quit` to quit.

#### Example Interactive Session

```
ULU> detect samples/
Detected languages: ['php', 'vb', 'cobol', 'java', 'bat']

ULU> analyze samples/legacy_php.php
Analysis: {'functions': 5, 'classes': 0, 'variables': 10, 'complexity': 'medium'}

ULU> simulate samples/legacy_php.php --target php8
Migration simulation: Code would be migrated successfully to PHP 8

ULU> migrate samples/legacy_php.php --target php8
Migration result: Code migrated successfully to PHP 8

ULU> validate samples/
Validation: Project structure is valid, all files migrated correctly

ULU> security samples/
Security analysis: No secrets detected, 2 vulnerabilities found

ULU> cleanup samples/
Cleanup: Project normalized, temporary files removed

ULU> version
Universal Legacy Updater version 1.0.0

ULU> exit
```

## Usage Examples

### Migrating a PHP 5 File

Input file (`samples/legacy_php.php`):

```php
<?php
mysql_connect('localhost', 'user', 'pass');
$query = mysql_query("SELECT * FROM users");
while ($row = mysql_fetch_array($query)) {
    echo $row['name'];
}
?>
```

After migration to PHP 8:

```php
<?php
$mysqli = new mysqli('localhost', 'user', 'pass', 'database');
$query = $mysqli->query("SELECT * FROM users");
while ($row = $query->fetch_assoc()) {
    echo $row['name'];
}
?>
```

### Migrating VB Script to C#

Input file (`samples/legacy_vb.vb`):

```vb
Dim x As Integer = 5
If x > 3 Then
    MsgBox("Hello World")
End If
```

After migration to C#:

```csharp
int x = 5;
if (x > 3)
{
    Console.WriteLine("Hello World");
}
```

## Architecture

The system is modular with the following components:

- **CLI Interface**: Entry point for user commands.
- **Detector**: Identifies legacy codebases and languages.
- **Analyzer**: Parses code, extracts business logic and structures.
- **Migrator**: Applies transformation rules to generate modern code.
- **Reporter**: Generates migration reports.

## Technologies Used

- **Primary Language**: Python 3.9+
- **Parsing and AST**: Language-specific parsers (e.g., php-ast for PHP, ANTLR for COBOL/VB)
- **Code Generation**: Jinja2 templates
- **CLI Framework**: Click
- **Database**: SQLite for metadata
- **Testing**: Pytest

## Contributing

Contributions are welcome. Please submit issues or pull requests on the GitHub repository.

## License

This project is licensed under the MIT License.