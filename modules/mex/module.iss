;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of the Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "mex"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsMex.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_MEX};
Source: {#RootPath}bin\{#BinPath}\libno-nlsMex.dll; DestDir: {app}\bin\{#BinPath}\;DestName: libnlsMex.dll; Components: not {#COMPONENT_MEX};
Source: {#RootPath}bin\{#BinPath}\libnlsMex.lib; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_MEX};
Source: {#RootPath}bin\{#BinPath}\libnlsMex_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_MEX};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\src\include\*.h; DestDir: {app}\modules\{#MODULE_NAME}\src\include\;Components: {#COMPONENT_MEX};
Source: {#RootPath}modules\{#MODULE_NAME}\src\include\*.hpp; DestDir: {app}\modules\{#MODULE_NAME}\src\include\;Components: {#COMPONENT_MEX};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_MEX};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_MEX};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_MEX};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\;Components: {#COMPONENT_MEX};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.qch; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_MEX} and {#COMPONENT_HELP_FILES};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_MEX} and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.c; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_MEX} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\examples\*.m; DestDir: {app}\modules\{#MODULE_NAME}\examples\;Components: {#COMPONENT_MEX};
Source: {#RootPath}modules\{#MODULE_NAME}\examples\*.c; DestDir: {app}\modules\{#MODULE_NAME}\examples\;Components: {#COMPONENT_MEX};
;==============================================================================
