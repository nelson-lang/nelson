popd
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
rem Parse optional architecture parameter (default x64)
set ARCH=%~1
if "%ARCH%"=="" set ARCH=x64

rem Map arch to bin subdirectory name
if /I "%ARCH%"=="arm64" (
    set BIN_SUB=ARM64
) else (
    set BIN_SUB=x64
)

rem Resolve BIN_DIR to absolute path
pushd "%~dp0..\..\bin\%BIN_SUB%" 2>nul
if errorlevel 1 (
    echo ERROR: Bin directory not found: "%~dp0..\..\bin\%BIN_SUB%"
    exit /b 1
)
set BIN_DIR=%CD%

rem =============================================================================
rem remove qt debug symbols
del "%BIN_DIR%\plugins\*.pdb" /s >nul 2>&1
del "%BIN_DIR%\plugins\*d.dll" /s >nul 2>&1
del "%BIN_DIR%\qml\*.pdb" /s >nul 2>&1
del "%BIN_DIR%\qml\*d.dll" /s >nul 2>&1
del "%BIN_DIR%\Qt*d.dll" /s >nul 2>&1
rmdir "%BIN_DIR%\plugins\bearer" /s /Q  >nul 2>&1
rmdir "%BIN_DIR%\plugins\designer" /s /Q  >nul 2>&1
rmdir "%BIN_DIR%\plugins\qmltooling" /s /Q >nul 2>&1
rmdir "%BIN_DIR%\plugins\position" /s /Q >nul 2>&1
rmdir "%BIN_DIR%\plugins\playlistformats" /s /Q  >nul 2>&1
rmdir "%BIN_DIR%\plugins\mediaservice" /s /Q >nul 2>&1
rmdir "%BIN_DIR%\plugins\geoservices" /s /Q >nul 2>&1
rmdir "%BIN_DIR%\plugins\audio" /s /Q >nul 2>&1
rmdir "%BIN_DIR%\plugins\generic" /s /Q >nul 2>&1
rmdir "%BIN_DIR%\qml\QtBluetooth" /s /Q >nul 2>&1
rmdir "%BIN_DIR%\qml\QtLocation" /s /Q >nul 2>&1
rmdir "%BIN_DIR%\qml\QtNfc" /s /Q >nul 2>&1
rmdir "%BIN_DIR%\qml\QtPositioning" /s /Q >nul 2>&1
rmdir "%BIN_DIR%\qml\QtTest" /s /Q >nul 2>&1
rem =============================================================================

