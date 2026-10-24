;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "optimization"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsOptimization.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_OPTIMIZATION};
Source: {#RootPath}bin\{#BinPath}\libnlsOptimization_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_OPTIMIZATION};
Source: {#RootPath}bin\{#BinPath}\highs.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_OPTIMIZATION};
Source: {#RootPath}bin\{#BinPath}\z.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_OPTIMIZATION};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_OPTIMIZATION};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_OPTIMIZATION};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_OPTIMIZATION};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\;Components: {#COMPONENT_OPTIMIZATION};
Source: {#RootPath}modules\{#MODULE_NAME}\functions\private\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\private\;Components: {#COMPONENT_OPTIMIZATION};
Source: {#RootPath}modules\{#MODULE_NAME}\functions\+optim\+options\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\+optim\+options\;Components: {#COMPONENT_OPTIMIZATION};
Source: {#RootPath}modules\{#MODULE_NAME}\functions\+optim\+options\private\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\+optim\+options\private\;Components: {#COMPONENT_OPTIMIZATION};
Source: {#RootPath}modules\{#MODULE_NAME}\functions\+optim\+problemdef\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\+optim\+problemdef\;Components: {#COMPONENT_OPTIMIZATION};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\examples\*; DestDir: {app}\modules\{#MODULE_NAME}\examples\; Flags: recursesubdirs;Components: {#COMPONENT_OPTIMIZATION};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.nhz; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_OPTIMIZATION} and {#COMPONENT_HELP_FILES};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_OPTIMIZATION} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
