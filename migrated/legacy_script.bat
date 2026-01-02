@echo off
REM Legacy DOS batch script with obsolete syntax and bad practices
REM No error handling, hardcoded paths, insecure operations

echo Starting legacy process...

REM Bad practice: no quotes around paths
copy c:\data\input.txt c:\output\result.txt

REM Hardcoded credentials in plain text - vulnerability
net use z: \\server\share /user:admin password123

REM Old DOS commands, no modern alternatives
fdisk /status

REM No input validation
if "%1"=="delete" del c:\important\file.txt

REM GOTO bad practice
goto end

:end
echo Done.