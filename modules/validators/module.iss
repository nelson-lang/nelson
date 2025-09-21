;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "validators"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsValidators.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_VALIDATORS};
Source: {#RootPath}bin\{#BinPath}\libnlsValidators.lib; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_VALIDATORS} and {#COMPONENT_DYNAMIC_LINK};
Source: {#RootPath}bin\{#BinPath}\libnlsValidators_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_VALIDATORS};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\src\include\*.h; DestDir: {app}\modules\{#MODULE_NAME}\src\include\;Components: {#COMPONENT_VALIDATORS} and {#COMPONENT_DYNAMIC_LINK};
Source: {#RootPath}modules\{#MODULE_NAME}\src\include\*.hpp; DestDir: {app}\modules\{#MODULE_NAME}\src\include\;Components: {#COMPONENT_VALIDATORS} and {#COMPONENT_DYNAMIC_LINK};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_VALIDATORS};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_VALIDATORS};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_VALIDATORS};
;==============================================================================
;Source: {#RootPath}modules\validators\functions\*.m; DestDir: {app}\modules\validators\functions\;Components: {#COMPONENT_VALIDATORS};
;==============================================================================
Source: {#RootPath}modules\validators\help\*.nhz; DestDir: {app}\modules\validators\help\; Flags: recursesubdirs;Components: {#COMPONENT_VALIDATORS} and {#COMPONENT_HELP_FILES};
;==============================================================================
Source: {#RootPath}modules\validators\tests\*.m; DestDir: {app}\modules\validators\tests\; Flags: recursesubdirs;Components: {#COMPONENT_VALIDATORS} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
