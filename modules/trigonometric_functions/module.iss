;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "trigonometric_functions"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsTrigonometric_functions.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_TRIGONOMETRIC_FUNCTIONS};
Source: {#RootPath}bin\{#BinPath}\libnlsTrigonometric_functions_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_TRIGONOMETRIC_FUNCTIONS};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_TRIGONOMETRIC_FUNCTIONS};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_TRIGONOMETRIC_FUNCTIONS};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_TRIGONOMETRIC_FUNCTIONS};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\;Components: {#COMPONENT_TRIGONOMETRIC_FUNCTIONS};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.qch; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_TRIGONOMETRIC_FUNCTIONS} and {#COMPONENT_HELP_FILES} and {#COMPONENT_HELP_BROWSER};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_TRIGONOMETRIC_FUNCTIONS} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
