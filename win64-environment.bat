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
if not defined QTDIR64 (
  set QTDIR64=%BAT_PATH%\..\qt_windows_x64\6.9.0\msvc2022_64
)
if not exist %QTDIR64% (
    echo %QTDIR64% not found.
    exit /b 1
)
set QTDIR=%QTDIR64%
rem =============================================================================
start NelSon.sln
