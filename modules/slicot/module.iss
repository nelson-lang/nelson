;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of the Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "slicot"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libslicot.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_SLICOT}; Flags: skipifsourcedoesntexist
Source: {#RootPath}bin\{#BinPath}\libnlsSlicot.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_SLICOT}
Source: {#RootPath}bin\{#BinPath}\libnlsSlicot_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_SLICOT}
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_SLICOT}
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_SLICOT}
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_SLICOT}
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\;Components: {#COMPONENT_SLICOT}
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.qch; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_SLICOT} and {#COMPONENT_HELP_FILES} and {#COMPONENT_HELP_BROWSER};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_SLICOT} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
