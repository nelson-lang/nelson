;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "nflow_gui"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsNflow_gui.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_NFLOW};
Source: {#RootPath}bin\{#BinPath}\libnlsNflow_gui_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_NFLOW};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_NFLOW};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_NFLOW};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_NFLOW};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\; Flags: recursesubdirs;Components: {#COMPONENT_NFLOW};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.nhz; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_NFLOW} and {#COMPONENT_HELP_FILES};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\dist\*; DestDir: {app}\modules\{#MODULE_NAME}\dist\; Flags: recursesubdirs;Components: {#COMPONENT_NFLOW};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\resources\*.*; DestDir: {app}\modules\{#MODULE_NAME}\resources\;Components: {#COMPONENT_NFLOW};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\examples\*; DestDir: {app}\modules\{#MODULE_NAME}\examples\; Flags: recursesubdirs;Components: {#COMPONENT_NFLOW};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_NFLOW} and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.nflow; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_NFLOW} and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.json; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_NFLOW} and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.nblock; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_NFLOW} and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.c; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_NFLOW} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
