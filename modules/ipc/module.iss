;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "ipc"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsIpc.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_IPC};
Source: {#RootPath}bin\{#BinPath}\libno-nlsIpc.dll; DestDir: {app}\bin\{#BinPath}\;DestName:libnlsIpc.dll;Components: not {#COMPONENT_IPC};
Source: {#RootPath}bin\{#BinPath}\libnlsIpc_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_IPC};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_IPC};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_IPC};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_IPC};
;==============================================================================
;Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\;Components: {#COMPONENT_IPC};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.qch; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_IPC} and {#COMPONENT_HELP_FILES} and {#COMPONENT_HELP_BROWSER};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_IPC} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
;Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.ref; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_IPC} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
