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
rem Launcher dispatcher: a leading mode selector picks the matching executable.
rem   --gui (default) -> nelson-gui.exe      (own window)
rem   --cli           -> nelson-cli.exe      (console)
rem   --adv-cli       -> nelson-adv-cli.exe  (console)
rem   --webview       -> nelson-webview.exe  (own window)
rem   --web           -> nelson-webview.exe  (console, forwarded to the exe)
rem Any other argument is forwarded to the selected executable.
rem =============================================================================
SETLOCAL ENABLEEXTENSIONS
SET "PARENT=%~dp0"
SET "NELSON_MODE=-gui"
SET "START_CMD=start "Nelson" "

:LOOP
SET "CURRENT_ARG=%~1"

IF "%CURRENT_ARG%"=="--cli" (
    SET "NELSON_MODE=-cli"
    SET "START_CMD="
    GOTO NEXT
)

IF "%CURRENT_ARG%"=="--adv-cli" (
    SET "NELSON_MODE=-adv-cli"
    SET "START_CMD="
    GOTO NEXT
)

IF "%CURRENT_ARG%"=="--gui" (
    SET "NELSON_MODE=-gui"
    GOTO NEXT
)

IF "%CURRENT_ARG%"=="--webview" (
    SET "NELSON_MODE=-webview"
    GOTO NEXT
)

IF "%CURRENT_ARG%"=="--web" (
    SET "NELSON_MODE=-webview"
    SET "START_CMD="
    SET FILTERED_ARGS=%FILTERED_ARGS% %1%
    GOTO NEXT
)

SET FILTERED_ARGS=%FILTERED_ARGS% %1%

:NEXT
shift

IF not "%CURRENT_ARG%"=="" GOTO LOOP

SET "PARENT=%PARENT:"=%"
rem Fall back to the console CLI when the selected executable is not installed
rem (for example a CLI-only / minimal install has no GUI, adv-cli or webview exe).
IF NOT EXIST "%PARENT%nelson%NELSON_MODE%.exe" (
    SET "NELSON_MODE=-cli"
    SET "START_CMD="
)
SET ERRORLEVEL=
%START_CMD%"%PARENT%nelson%NELSON_MODE%.exe"%FILTERED_ARGS%

EXIT /B %ERRORLEVEL%
