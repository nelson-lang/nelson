@echo off
rem =============================================================================
rem Copyright (c) 2016-present Allan CORNET (Nelson)
rem =============================================================================
rem This file is part of the Nelson.
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
if not defined QTDIRARM64 (
  for %%i in ("%BAT_PATH%\..\qt_windows_arm64\6.9.0\msvc2022_64") do set QTDIRARM64=%%~fi
)
rem =============================================================================
if not exist "%QTDIRARM64%" (
    echo %QTDIRARM64% not found.
    exit /b 1
)
set QTDIR=%QTDIRARM64%
rem =============================================================================
if "%~1"=="" (
    echo Starting NelSon.sln...
    start NelSon.sln
    exit /b 0
)
rem =============================================================================
rem Check for the "env-only" parameter
if /i "%~1"=="env-only" (
    echo Environment initialized only (QTDIR=%QTDIRARM64%)
    exit /b 0
)
rem =============================================================================