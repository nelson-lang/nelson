;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of the Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "stream_manager"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsStream_manager.lib; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}modules\{#MODULE_NAME}\src\include\*.h; DestDir: {app}\modules\{#MODULE_NAME}\src\include\;
Source: {#RootPath}modules\{#MODULE_NAME}\src\include\*.hpp; DestDir: {app}\modules\{#MODULE_NAME}\src\include\;
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsStream_manager.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\libnlsStream_manager_builtin.dll; DestDir: {app}\bin\{#BinPath}\;
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\;
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.qch; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs
Source: {#RootPath}modules\{#MODULE_NAME}\tests\loadsavebin\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\loadsavebin\; Flags: recursesubdirs
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.bin; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.json; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.txt; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs
;==============================================================================
