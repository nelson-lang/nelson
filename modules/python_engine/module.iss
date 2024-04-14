;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of the Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "python_engine"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsPython_engine.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_PYTHON_ENGINE};
Source: {#RootPath}bin\{#BinPath}\libnlsPython_engine_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_PYTHON_ENGINE};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_PYTHON_ENGINE};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_PYTHON_ENGINE};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_PYTHON_ENGINE};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\;Components: {#COMPONENT_PYTHON_ENGINE};
Source: {#RootPath}modules\{#MODULE_NAME}\functions\@pyargs\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\@pyargs;Components: {#COMPONENT_PYTHON_ENGINE};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\resources\python\*.py; DestDir: {app}\modules\{#MODULE_NAME}\resources\python\;Components: {#COMPONENT_PYTHON_ENGINE};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.qch; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_PYTHON_ENGINE} and {#COMPONENT_HELP_FILES} and {#COMPONENT_HELP_BROWSER};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_PYTHON_ENGINE} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\thirdparty\python\*.*; DestDir: {app}\modules\{#MODULE_NAME}\thirdparty\python\;Flags: recursesubdirs skipifdoesntexist;Components: {#COMPONENT_PYTHON_RUNTIME};
;==============================================================================
