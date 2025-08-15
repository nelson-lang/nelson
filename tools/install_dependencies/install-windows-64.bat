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
pushd "%BAT_PATH%\..\..\..\qt_windows_x64\6.9.1\msvc2022_64" 2>nul
if errorlevel 1 (
    echo ERROR: Qt directory does not exist: "%BAT_PATH%\..\..\..\qt_windows_x64\6.9.1\msvc2022_64"
    cd "%ORIGINAL_DIR%"
    exit /b 1
)
set QTDIR64=%CD%
popd
echo QTDIR64 resolved to: %QTDIR64%
rem =============================================================================
rem Check and execute the install.bat script
set INSTALL_SCRIPT=%BAT_PATH%\..\..\..\nelson-thirdparty-x64\install.bat
if exist "%INSTALL_SCRIPT%" (
    echo Found install.bat. Executing the script...
    cd "%BAT_PATH%\..\..\..\nelson-thirdparty-x64"
    call "install.bat"
    if errorlevel 1 (
        echo The install.bat script encountered an error during execution.
        cd "%ORIGINAL_DIR%"
        exit /b 1
    )
) else (
    echo ERROR: The install.bat file does not exist in the expected location: "%INSTALL_SCRIPT%"
    echo Please check the repository structure or clone the repository again.
    cd "%ORIGINAL_DIR%"
    exit /b 1
)
rem =============================================================================
rem Check for optional parameter "remove-debug"
if "%1"=="remove-debug" (
    set REMOVE_DEBUG_SCRIPT=%BAT_PATH%\..\..\..\nelson-thirdparty-x64\remove_debug.bat
    echo "remove-debug" parameter detected. Calling remove-debug.bat...
    if exist "%BAT_PATH%\..\..\..\nelson-thirdparty-x64\remove_debug.bat" (
        echo Found remove_debug. Executing the script...
        cd "%BAT_PATH%\..\..\..\nelson-thirdparty-x64"
        call "remove_debug.bat"
        if errorlevel 1 (
            echo remove_debug.bat encountered an error during execution.
            cd "%ORIGINAL_DIR%"
            exit /b 1
        )
    ) else (
        echo ERROR: remove_debug.bat does not exist in the expected location: "%BAT_PATH%\remove-debug.bat"
        cd "%ORIGINAL_DIR%"
        exit /b 1
    )
)
rem =============================================================================
rem Restore the original directory on successful completion
cd "%ORIGINAL_DIR%"
rem =============================================================================
