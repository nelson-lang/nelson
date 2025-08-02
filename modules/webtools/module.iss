;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "webtools"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libcurl.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_WEBTOOLS};
Source: {#RootPath}bin\{#BinPath}\git2.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_WEBTOOLS};
Source: {#RootPath}bin\{#BinPath}\pcre.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_WEBTOOLS};
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsWebtools.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_WEBTOOLS};
Source: {#RootPath}bin\{#BinPath}\libnlsWebtools_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_WEBTOOLS};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_WEBTOOLS};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_WEBTOOLS};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_WEBTOOLS};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\;Components: {#COMPONENT_WEBTOOLS};
Source: {#RootPath}modules\{#MODULE_NAME}\functions\@weboptions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\@weboptions\;Components: {#COMPONENT_WEBTOOLS};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.qch; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_WEBTOOLS};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_WEBTOOLS} and {#COMPONENT_HELP_FILES} and {#COMPONENT_HELP_BROWSER};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.json; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_WEBTOOLS} and {#COMPONENT_HELP_FILES} and {#COMPONENT_HELP_BROWSER};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\examples\*.*; DestDir: {app}\modules\{#MODULE_NAME}\examples\; Flags: recursesubdirs;Components: {#COMPONENT_WEBTOOLS} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
