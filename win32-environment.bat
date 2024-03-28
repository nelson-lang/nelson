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
set QTDIR=%QTDIR32%
if not defined QTDIR (
    echo QTDIR64 not defined.
    exit /b 1
)
rem =============================================================================
start NelSon.sln
