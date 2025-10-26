;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "tests_manager"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsTests_manager.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_TESTS_MANAGER};
Source: {#RootPath}bin\{#BinPath}\libnlsTests_manager_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_TESTS_MANAGER};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_TESTS_MANAGER};
Source: {#RootPath}modules\{#MODULE_NAME}\minimal_tests.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_TESTS_MANAGER};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_TESTS_MANAGER};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_TESTS_MANAGER};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\;Components: {#COMPONENT_TESTS_MANAGER};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.nhz; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_HELP_FILES};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
