;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of the Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "help_tools"
;==============================================================================
; xml libraries
Source: {#RootPath}bin\{#BinPath}\iconv-2.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\charset-1.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\libxml2.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\lzma.dll; DestDir: {app}\bin\{#BinPath}\;
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsHelp_tools.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\libnlsHelp_tools_builtin.dll; DestDir: {app}\bin\{#BinPath}\;
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\resources\about.txt; DestDir: {app}\modules\{#MODULE_NAME}\resources\;
Source: {#RootPath}modules\{#MODULE_NAME}\resources\highlight.pack.js; DestDir: {app}\modules\{#MODULE_NAME}\resources\;
Source: {#RootPath}modules\{#MODULE_NAME}\resources\mono-blue.css; DestDir: {app}\modules\{#MODULE_NAME}\resources\;
Source: {#RootPath}modules\{#MODULE_NAME}\resources\nelson_help_collection.qhc; DestDir: {app}\modules\{#MODULE_NAME}\resources\;
Source: {#RootPath}modules\{#MODULE_NAME}\resources\style.css; DestDir: {app}\modules\{#MODULE_NAME}\resources\;
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\;
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.qch; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\xml\*.xml; DestDir: {app}\modules\{#MODULE_NAME}\tests\xml; Flags: recursesubdirs
;==============================================================================
