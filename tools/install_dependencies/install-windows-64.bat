@echo off
rem =============================================================================
rem Copyright (c) 2016-present Allan CORNET (Nelson)
rem =============================================================================
rem This file is part of Nelson.
rem =============================================================================
rem # LICENCE_BLOCK_BEGIN
rem # SPDX-License-Identifier: LGPL-3.0-or-later
rem # LICENCE_BLOCK_END
rem =============================================================================
rem Save the current directory
set ORIGINAL_DIR=%CD%
rem =============================================================================
rem Set the script's directory as the root path
set BAT_PATH=%~dp0
rem Remove trailing backslash if present
set BAT_PATH=%BAT_PATH:~0,-1%
rem =============================================================================
rem Check if git is available
git --version >nul 2>&1
if errorlevel 1 (
    echo Git is not installed or not available in PATH.
    cd "%ORIGINAL_DIR%"
    exit /b 1
)
rem =============================================================================
rem Install the Node.js version required by nflow_gui with nvm-windows
set /p NODE_VERSION=<"%BAT_PATH%\..\..\.nvmrc"
nvm version >nul 2>&1
if errorlevel 1 (
    echo nvm-windows is not installed or not available in PATH. Installing with winget...
    winget install --id CoreyButler.NVMforWindows --exact
    if errorlevel 1 (
        echo Failed to install nvm-windows. Please install nvm-windows manually.
        cd "%ORIGINAL_DIR%"
        exit /b 1
    )
)
if exist "%ProgramFiles%\nvm\nvm.exe" set "PATH=%ProgramFiles%\nvm;%PATH%"
if exist "%APPDATA%\nvm\nvm.exe" set "PATH=%APPDATA%\nvm;%PATH%"
nvm version >nul 2>&1
if errorlevel 1 (
    echo nvm-windows is not installed or not available in PATH.
    cd "%ORIGINAL_DIR%"
    exit /b 1
)
nvm install %NODE_VERSION%
if errorlevel 1 (
    echo Failed to install Node.js %NODE_VERSION% with nvm.
    cd "%ORIGINAL_DIR%"
    exit /b 1
)
nvm use %NODE_VERSION%
if errorlevel 1 (
    echo Failed to select Node.js %NODE_VERSION% with nvm.
    cd "%ORIGINAL_DIR%"
    exit /b 1
)
rem =============================================================================
rem Ensure a directory that really contains node.exe is on PATH.
rem "nvm use" exposes node through a directory symlink (mklink /D), which needs
rem admin/Developer Mode and is not reliably present right after "nvm use"; a
rem shell opened before nvm was configured also won't have it on PATH. So we do
rem NOT rely on that symlink: we prepend the actual version directory created by
rem "nvm install" (%NVM_DIR%\v<version>), which always exists. NODE_VERSION may
rem be a major alias (e.g. "22"), so fall back to matching "v<version>*".
rem Single-line "if" statements are used on purpose (variables set inside a
rem parenthesized block are not visible in the same block without delayed
rem expansion). Non-blocking: warn and continue if node.exe cannot be located.
set "NVM_DIR=%NVM_HOME%"
if not defined NVM_DIR for /f "delims=" %%N in ('where nvm 2^>nul') do if not defined NVM_DIR set "NVM_DIR=%%~dpN"
if defined NVM_DIR if "%NVM_DIR:~-1%"=="\" set "NVM_DIR=%NVM_DIR:~0,-1%"
set "NVM_NODE_DIR="
if defined NVM_DIR if exist "%NVM_DIR%\v%NODE_VERSION%\node.exe" set "NVM_NODE_DIR=%NVM_DIR%\v%NODE_VERSION%"
if not defined NVM_NODE_DIR if defined NVM_DIR for /f "delims=" %%D in ('dir /b /ad "%NVM_DIR%\v%NODE_VERSION%*" 2^>nul') do if exist "%NVM_DIR%\%%D\node.exe" set "NVM_NODE_DIR=%NVM_DIR%\%%D"
if not defined NVM_NODE_DIR if defined NVM_SYMLINK if exist "%NVM_SYMLINK%\node.exe" set "NVM_NODE_DIR=%NVM_SYMLINK%"
if defined NVM_NODE_DIR set "PATH=%NVM_NODE_DIR%;%PATH%"
if not defined NVM_NODE_DIR echo WARNING: Could not locate node.exe under nvm; relying on the existing PATH to locate node.
node --version >nul 2>&1
if errorlevel 1 (
    echo Node.js is not installed or not available in PATH.
    cd "%ORIGINAL_DIR%"
    exit /b 1
)
rem "call" is required: npm is npm.cmd (a batch script); without "call" the
rem parent batch would hand control to npm.cmd and never resume (clone/copy
rem steps below would be skipped).
call npm --version >nul 2>&1
if errorlevel 1 (
    echo npm is not installed or not available in PATH.
    cd "%ORIGINAL_DIR%"
    exit /b 1
)
rem =============================================================================
rem Clone or update the repository
if exist "%BAT_PATH%\..\..\..\nelson-thirdparty-x64\" (
    if exist "%BAT_PATH%\..\..\..\nelson-thirdparty-x64\.git" (
        echo Repository already exists. Pulling latest changes...
        cd "%BAT_PATH%\..\..\..\nelson-thirdparty-x64"
        git pull
        if errorlevel 1 (
            echo Failed to pull the latest changes. Please check your internet connection or repository state.
            cd "%ORIGINAL_DIR%"
            exit /b 1
        )
        cd "%BAT_PATH%"
    ) else (
        echo The directory nelson-thirdparty-x64 exists but is not a valid git repository.
        echo Please remove the directory and try again.
        cd "%ORIGINAL_DIR%"
        exit /b 1
    )
) else (
    echo Cloning the repository...
    git clone https://github.com/nelson-lang/nelson-thirdparty-x64.git %BAT_PATH%\..\..\..\nelson-thirdparty-x64
    if errorlevel 1 (
        echo Failed to clone the repository. Please check your internet connection or SSH configuration.
        cd "%ORIGINAL_DIR%"
        exit /b 1
    )
)
rem =============================================================================
rem Clone or update the qt6
if exist "%BAT_PATH%\..\..\..\qt_windows_x64\" (
    if exist "%BAT_PATH%\..\..\..\qt_windows_x64\.git" (
        echo Repository Qt already exists. Pulling latest changes...
        cd "%BAT_PATH%\..\..\..\qt_windows_x64"
        git pull
        if errorlevel 1 (
            echo Failed to pull the latest changes. Please check your internet connection or repository state.
            cd "%ORIGINAL_DIR%"
            exit /b 1
        )
        cd "%BAT_PATH%"
    ) else (
        echo The directory qt_windows_x64 exists but is not a valid git repository.
        echo Please remove the directory and try again.
        cd "%ORIGINAL_DIR%"
        exit /b 1
    )
) else (
    echo Cloning the repository...
    git clone https://github.com/nelson-lang/qt_windows_x64.git %BAT_PATH%\..\..\..\qt_windows_x64
    if errorlevel 1 (
        echo Failed to clone the repository. Please check your internet connection or SSH configuration.
        cd "%ORIGINAL_DIR%"
        exit /b 1
    )
)
rem =============================================================================
rem Resolve QTDIR64 to absolute path by temporarily changing directory
pushd "%BAT_PATH%\..\..\..\qt_windows_x64\6.11.0\msvc2022_64" 2>nul
if errorlevel 1 (
    echo ERROR: Qt directory does not exist: "%BAT_PATH%\..\..\..\qt_windows_x64\6.11.0\msvc2022_64"
    cd "%ORIGINAL_DIR%"
    exit /b 1
)
set QTDIR64=%CD%
popd
echo QTDIR64 resolved to: %QTDIR64%
rem =============================================================================
rem Install dll files in bin directory (x64)
call "%BAT_PATH%\copy-dependencies-x64.bat"
if errorlevel 1 (
    echo Failed to copy DLL files.
    cd "%ORIGINAL_DIR%"
    exit /b 1
)
rem Copy optional tooling moved into copy-dependencies-x64.bat

rem =============================================================================
rem Check for optional parameter "remove-debug"
if "%1"=="remove-debug" (
    echo "remove-debug" parameter detected. Calling qt-remove-debug.bat...
    if exist "%BAT_PATH%\qt-remove-debug.bat" (
        echo Found qt-remove-debug.bat. Executing the script...
        call "%BAT_PATH%\qt-remove-debug.bat" x64
        if errorlevel 1 (
            echo qt-remove-debug.bat encountered an error during execution.
            cd "%ORIGINAL_DIR%"
            exit /b 1
        )
    ) else (
        echo ERROR: qt-remove-debug.bat does not exist in the expected location: "%BAT_PATH%\qt-remove-debug.bat"
        cd "%ORIGINAL_DIR%"
        exit /b 1
    )
)
rem =============================================================================
rem Restore the original directory on successful completion
cd "%ORIGINAL_DIR%"
rem =============================================================================
