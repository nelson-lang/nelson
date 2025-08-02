;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "characters_encoding"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\icudt74.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_INTERNATIONALIZATION};
Source: {#RootPath}bin\{#BinPath}\icuuc74.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_INTERNATIONALIZATION};
Source: {#RootPath}bin\{#BinPath}\icuin74.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_INTERNATIONALIZATION};
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsCharacters_encoding.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_INTERNATIONALIZATION};
Source: {#RootPath}bin\{#BinPath}\libnlsCharacters_encoding_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_INTERNATIONALIZATION};
Source: {#RootPath}bin\{#BinPath}\libno-nlsCharacters_encoding.dll; DestDir: {app}\bin\{#BinPath}\;DestName:libnlsCharacters_encoding.dll;Components: not {#COMPONENT_INTERNATIONALIZATION};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\src\include\*.h; DestDir: {app}\modules\{#MODULE_NAME}\src\include\;
Source: {#RootPath}modules\{#MODULE_NAME}\src\include\*.hpp; DestDir: {app}\modules\{#MODULE_NAME}\src\include\;
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_INTERNATIONALIZATION};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_INTERNATIONALIZATION};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_INTERNATIONALIZATION};
;==============================================================================
;Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\;Components: {#COMPONENT_INTERNATIONALIZATION};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.qch; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_INTERNATIONALIZATION} and {#COMPONENT_HELP_BROWSER} and {#COMPONENT_HELP_FILES};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_INTERNATIONALIZATION} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.txt; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_INTERNATIONALIZATION} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
;Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.ref; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_INTERNATIONALIZATION} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
