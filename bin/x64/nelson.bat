@echo off
rem =============================================================================
rem Copyright (c) 2016-present Allan CORNET (Nelson)
rem =============================================================================
rem This file is part of the Nelson.
rem =============================================================================
rem # LICENCE_BLOCK_BEGIN
rem # This program is free software; you can redistribute it and/or
rem # modify it under the terms of the GNU Lesser General Public
rem # License as published by the Free Software Foundation; either
rem # version 2.1 of the License, or (at your option) any later version.
rem #
rem # Alternatively, you can redistribute it and/or
rem # modify it under the terms of the GNU General Public License as
rem # published by the Free Software Foundation; either version 2 of
rem # the License, or (at your option) any later version.
rem #
rem # This program is distributed in the hope that it will be useful,
rem # but WITHOUT ANY WARRANTY; without even the implied warranty of
rem # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
rem # GNU Lesser General Public License for more details.
rem #
rem # You should have received a copy of the GNU Lesser General Public
rem # License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
set NELSON_MODE=%NELSON_MODE:"=%
%START_CMD%"%PARENT%nelson%NELSON_MODE%.exe"%FILTERED_ARGS%

EXIT /B %ERRORLEVEL%
