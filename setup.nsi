; UniversalLegacyUpdater NSIS Installer Script
; This script creates a professional installer for UniversalLegacyUpdater

!include "MUI2.nsh"
!include "FileFunc.nsh"
!include "LogicLib.nsh"

; General Configuration
Name "UniversalLegacyUpdater"
OutFile "UniversalLegacyUpdater_Setup.exe"
Unicode True
InstallDir "$PROGRAMFILES\UniversalLegacyUpdater"
InstallDirRegKey HKCU "Software\UniversalLegacyUpdater" ""
RequestExecutionLevel admin

; Modern UI Configuration
!define MUI_ABORTWARNING

; Pages
!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH

!insertmacro MUI_UNPAGE_WELCOME
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES
!insertmacro MUI_UNPAGE_FINISH

; Languages
!insertmacro MUI_LANGUAGE "English"

; Version Information
VIProductVersion "1.0.0.0"
VIAddVersionKey "ProductName" "UniversalLegacyUpdater"
VIAddVersionKey "CompanyName" "Azur"
VIAddVersionKey "FileVersion" "1.0.0"
VIAddVersionKey "ProductVersion" "1.0.0"
VIAddVersionKey "FileDescription" "Universal Legacy Code Updater Installer"

; Installer Sections
Section "Main" SecMain
    ; Check Python installation
    Call CheckPython
    Pop $0
    ${If} $0 != 0
        MessageBox MB_OK "Python 3.9+ is required but not found. Please install Python from https://python.org and run the installer again."
        Abort
    ${EndIf}

    ; Set output path
    SetOutPath "$INSTDIR"

    ; Copy all files
    File /r "*"

    ; Install Python dependencies
    Call InstallDependencies

    ; Add to PATH
    Call AddToPath

    ; Create shortcuts
    Call CreateShortcuts

    ; Write uninstaller
    WriteUninstaller "$INSTDIR\Uninstall.exe"

    ; Write registry keys
    WriteRegStr HKCU "Software\UniversalLegacyUpdater" "" $INSTDIR
    WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\UniversalLegacyUpdater" "DisplayName" "UniversalLegacyUpdater"
    WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\UniversalLegacyUpdater" "UninstallString" "$INSTDIR\Uninstall.exe"
    WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\UniversalLegacyUpdater" "DisplayVersion" "1.0.0"
    WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\UniversalLegacyUpdater" "NoModify" 1
    WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\UniversalLegacyUpdater" "NoRepair" 1
SectionEnd

; Uninstaller Section
Section "Uninstall"
    ; Remove from PATH
    Call un.RemoveFromPath

    ; Remove shortcuts
    Call un.RemoveShortcuts

    ; Remove files
    Delete "$INSTDIR\Uninstall.exe"
    RMDir /r "$INSTDIR"

    ; Remove registry keys
    DeleteRegKey HKCU "Software\UniversalLegacyUpdater"
    DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\UniversalLegacyUpdater"
SectionEnd

; Functions
Function CheckPython
    ; Try py command (Windows Python launcher)
    nsExec::ExecToLog 'py --version'
    Pop $0
    ${If} $0 == 0
        nsExec::ExecToLog 'py -c "import sys; sys.exit(0 if sys.version_info >= (3,9) else 1)"'
        Pop $0
        ${If} $0 == 0
            Push 0 ; OK
            Return
        ${EndIf}
    ${EndIf}

    ; Try python3 command
    nsExec::ExecToLog 'python3 --version'
    Pop $0
    ${If} $0 == 0
        nsExec::ExecToLog 'python3 -c "import sys; sys.exit(0 if sys.version_info >= (3,9) else 1)"'
        Pop $0
        ${If} $0 == 0
            Push 0 ; OK
            Return
        ${EndIf}
    ${EndIf}

    ; Try python command
    nsExec::ExecToLog 'python --version'
    Pop $0
    ${If} $0 == 0
        nsExec::ExecToLog 'python -c "import sys; sys.exit(0 if sys.version_info >= (3,9) else 1)"'
        Pop $0
        ${If} $0 == 0
            Push 0 ; OK
            Return
        ${EndIf}
    ${EndIf}

    ; No suitable Python found
    Push 1
FunctionEnd

Function InstallDependencies
    nsExec::ExecToLog 'pip install -r "$INSTDIR\requirements.txt"'
    Pop $0
    ${If} $0 != 0
        MessageBox MB_OK "Failed to install Python dependencies. Please check your internet connection."
    ${EndIf}
FunctionEnd

Function AddToPath
    ReadRegStr $0 HKCU "Environment" "PATH"
    ${If} $0 == ""
        StrCpy $0 "$INSTDIR"
    ${Else}
        StrCpy $0 "$0;$INSTDIR"
    ${EndIf}
    WriteRegStr HKCU "Environment" "PATH" $0
    SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} 0 "STR:Environment" /TIMEOUT=5000
FunctionEnd

Function CreateShortcuts
    CreateShortcut "$DESKTOP\ULU.lnk" "$INSTDIR\ulu.bat" "" "$INSTDIR\ulu.bat" 0
    CreateDirectory "$SMPROGRAMS\UniversalLegacyUpdater"
    CreateShortcut "$SMPROGRAMS\UniversalLegacyUpdater\ULU.lnk" "$INSTDIR\ulu.bat" "" "$INSTDIR\ulu.bat" 0
    CreateShortcut "$SMPROGRAMS\UniversalLegacyUpdater\Uninstall.lnk" "$INSTDIR\Uninstall.exe" "" "$INSTDIR\Uninstall.exe" 0
FunctionEnd

Function un.RemoveFromPath
    ReadRegStr $0 HKCU "Environment" "PATH"
    StrLen $1 $0
    StrLen $2 "$INSTDIR"
    ${If} $1 > $2
        IntOp $3 $1 - $2
        StrCpy $4 $0 $2 $3
        ${If} $4 == "$INSTDIR"
            StrCpy $5 $0 $3
            ${If} $5 == ";"
                StrCpy $0 $0 "" $3
            ${EndIf}
        ${EndIf}
    ${ElseIf} $0 == "$INSTDIR"
        StrCpy $0 ""
    ${EndIf}
    WriteRegStr HKCU "Environment" "PATH" $0
    SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} 0 "STR:Environment" /TIMEOUT=5000
FunctionEnd

Function un.RemoveShortcuts
    Delete "$DESKTOP\ULU.lnk"
    Delete "$SMPROGRAMS\UniversalLegacyUpdater\ULU.lnk"
    Delete "$SMPROGRAMS\UniversalLegacyUpdater\Uninstall.lnk"
    RMDir "$SMPROGRAMS\UniversalLegacyUpdater"
FunctionEnd