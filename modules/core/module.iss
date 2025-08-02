;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "core"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsCore.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\libnlsCore_builtin.dll; DestDir: {app}\bin\{#BinPath}\;
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\src\include\EvaluateCommand.hpp; DestDir: {app}\modules\{#MODULE_NAME}\src\include\;
Source: {#RootPath}modules\{#MODULE_NAME}\src\include\EvaluateInterface.hpp; DestDir: {app}\modules\{#MODULE_NAME}\src\include\;
Source: {#RootPath}modules\{#MODULE_NAME}\src\include\EvaluateScriptFile.hpp; DestDir: {app}\modules\{#MODULE_NAME}\src\include\;
Source: {#RootPath}modules\{#MODULE_NAME}\src\include\nlsCore_exports.h; DestDir: {app}\modules\{#MODULE_NAME}\src\include\;
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\;
Source: {#RootPath}modules\{#MODULE_NAME}\functions\@DisplayFormatOptions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\@DisplayFormatOptions\;
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.qch; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_HELP_BROWSER} and {#COMPONENT_HELP_FILES};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_UNIT_TESTS};
;==============================================================================
