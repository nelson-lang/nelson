;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "time"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsTime.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\libnlsTime_builtin.dll; DestDir: {app}\bin\{#BinPath}\;
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\;
Source: {#RootPath}modules\{#MODULE_NAME}\functions\@sparsedouble\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\@sparsedouble\;
Source: {#RootPath}modules\{#MODULE_NAME}\functions\@cell\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\@cell\;
Source: {#RootPath}modules\{#MODULE_NAME}\functions\@char\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\@char\;
Source: {#RootPath}modules\{#MODULE_NAME}\functions\@string\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\@string\;
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.qch; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_HELP_FILES} and {#COMPONENT_HELP_BROWSER};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
