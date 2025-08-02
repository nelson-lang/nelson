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
SETLOCAL ENABLEEXTENSIONS
SET PARENT=%~dp0
SET DEFAULT_NELSON_MODE="-gui"
SET NELSON_MODE=%DEFAULT_NELSON_MODE%
SET "START_CMD=start "Nelson" "

:LOOP
SET CURRENT_ARG=%~1

IF "%CURRENT_ARG%"=="-adv-cli" (
    SET NELSON_MODE="-adv-cli"
    SET START_CMD=
    GOTO NEXT
)

IF "%CURRENT_ARG%"=="-sio-cli" (
    SET NELSON_MODE="-sio-cli"
    SET START_CMD=
    GOTO NEXT
)

IF "%CURRENT_ARG%"=="-cli" (
    SET NELSON_MODE="-cli"
    SET START_CMD=
    GOTO NEXT
)

IF "%CURRENT_ARG%"=="-gui" (
    SET NELSON_MODE="-gui"
    GOTO NEXT
)

set FILTERED_ARGS=%FILTERED_ARGS% %1%

:NEXT
shift

IF not "%CURRENT_ARG%"=="" GOTO LOOP

set PARENT=%PARENT:"=%
set ERRORLEVEL=
set NELSON_MODE=%NELSON_MODE:"=%
%START_CMD%"%PARENT%nelson%NELSON_MODE%.exe"%FILTERED_ARGS%

EXIT /B %ERRORLEVEL%
