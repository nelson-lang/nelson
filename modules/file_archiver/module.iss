;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "file_archiver"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\zlib1.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_FILE_ARCHIVER};
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsFile_archiver.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_FILE_ARCHIVER};
Source: {#RootPath}bin\{#BinPath}\libno-nlsFile_archiver.dll; DestDir: {app}\bin\{#BinPath}\;DestName:libnlsFile_archiver.dll;Components: not {#COMPONENT_FILE_ARCHIVER};
Source: {#RootPath}bin\{#BinPath}\libnlsFile_archiver_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_FILE_ARCHIVER};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_FILE_ARCHIVER};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_FILE_ARCHIVER};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_FILE_ARCHIVER};
;==============================================================================
;Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\;Components: {#COMPONENT_FILE_ARCHIVER};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.qch; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_FILE_ARCHIVER} and {#COMPONENT_HELP_BROWSER} and {#COMPONENT_HELP_FILES};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_FILE_ARCHIVER} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.zip; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_FILE_ARCHIVER} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
