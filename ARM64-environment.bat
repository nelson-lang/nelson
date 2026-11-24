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
set "ORIGINAL_DIR=%CD%"
rem =============================================================================
rem Set the script's directory as the root path
set "BAT_PATH=%~dp0"
rem Remove trailing backslash if present
set "BAT_PATH=%BAT_PATH:~0,-1%"
rem =============================================================================
if "%~1"=="" (
    echo Starting nelson.sln...
    start nelson.sln
    exit /b 0
)
rem =============================================================================
if /i "%~1"=="minimal" (
    echo Starting nelson-minimal.slnf ^(29 projects^)...
    start nelson-minimal.slnf
    exit /b 0
)
if /i "%~1"=="core" (
    echo Starting nelson-core.slnf ^(95 projects^)...
    start nelson-core.slnf
    exit /b 0
)
if /i "%~1"=="nflow" (
    echo Starting nelson-nflow.slnf ^(111 projects^)...
    start nelson-nflow.slnf
    exit /b 0
)
rem =============================================================================
rem Check for the "env-only" parameter
if /i "%~1"=="env-only" (
    echo Environment initialized only.
    exit /b 0
)
rem =============================================================================
if /i "%~1"=="help" (
    echo Usage: %~nx0 [option]
    echo.
    echo Options:
    echo   ^(none^)     Open nelson.sln ^(all 195+ projects^)
    echo   minimal    Open nelson-minimal.slnf ^(29 projects: interpreter, types^)
    echo   core       Open nelson-core.slnf ^(95 projects: full CLI^)
    echo   nflow      Open nelson-nflow.slnf ^(111 projects: simulation^)
    echo   env-only   Set environment variables only
    echo   help       Show this help
    exit /b 0
)
rem =============================================================================
