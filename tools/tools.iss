;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
Source: {#RootPath}tools\tests_all\*.*; DestDir: {app}\tools\tests_all\;Components: {#COMPONENT_TESTS_MANAGER};
Source: {#RootPath}tools\benchmark_all\*.*; DestDir: {app}\tools\benchmark_all\;Components: {#COMPONENT_TESTS_MANAGER};
Source: {#RootPath}tools\tests_result\*.*; DestDir: {app}\tools\tests_result\;Components: {#COMPONENT_TESTS_MANAGER};
Source: {#RootPath}tools\module_CI\*.*; DestDir: {app}\tools\module_CI\;Components: {#COMPONENT_TESTS_MANAGER};
;==============================================================================
Source: {#RootPath}tools\cmake\bin\*.*; DestDir: {app}\tools\cmake\bin\; Flags: recursesubdirs;Components: {#COMPONENT_DYNAMIC_LINK};
Source: {#RootPath}tools\cmake\share\cmake-4.2\*.*; DestDir: {app}\tools\cmake\share\cmake-4.2\; Flags: recursesubdirs;Components: {#COMPONENT_DYNAMIC_LINK};
;==============================================================================
