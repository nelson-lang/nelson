;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of the Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
Source: {#RootPath}tools\gettext\bin\*.*; DestDir: {app}\tools\gettext\bin\;
Source: {#RootPath}tools\tests_all\*.*; DestDir: {app}\tools\tests_all\;
Source: {#RootPath}tools\benchs_all\*.*; DestDir: {app}\tools\benchs_all\;
Source: {#RootPath}tools\tests_result\*.*; DestDir: {app}\tools\tests_result\;
Source: {#RootPath}tools\module_CI\*.*; DestDir: {app}\tools\module_CI\;
Source: {#RootPath}tools\cmake\*.*; DestDir: {app}\tools\cmake\; Flags: recursesubdirs
;==============================================================================
