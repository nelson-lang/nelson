;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "nflow_engine"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsNflow_engine.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_NFLOW};
Source: {#RootPath}bin\{#BinPath}\libnlsNflow_engine_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_NFLOW};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_NFLOW};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_NFLOW};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_NFLOW};
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\; Flags: recursesubdirs;Components: {#COMPONENT_NFLOW};
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.nhz; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_NFLOW} and {#COMPONENT_HELP_FILES};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_NFLOW} and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.csv; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_NFLOW} and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.nflow; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_NFLOW} and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.json; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_NFLOW} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
