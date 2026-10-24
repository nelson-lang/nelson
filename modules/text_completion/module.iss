;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "text_completion"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsText_completion.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_TEXT_COMPLETION};
Source: {#RootPath}bin\{#BinPath}\libnlsText_completion_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_TEXT_COMPLETION};
Source: {#RootPath}bin\{#BinPath}\libno-nlsText_completion.dll; DestDir: {app}\bin\{#BinPath}\;DestName:libnlsText_completion.dll;Components: not {#COMPONENT_TEXT_COMPLETION};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_TEXT_COMPLETION};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_TEXT_COMPLETION};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_TEXT_COMPLETION};
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.nhz; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_TEXT_COMPLETION} and {#COMPONENT_HELP_FILES};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_TEXT_COMPLETION} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
