;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of the Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "control_system"
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_CONTROL_SYSTEM};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_CONTROL_SYSTEM};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_CONTROL_SYSTEM};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\;Components: {#COMPONENT_CONTROL_SYSTEM};
Source: {#RootPath}modules\{#MODULE_NAME}\functions\private\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\private\;Components: {#COMPONENT_CONTROL_SYSTEM};
Source: {#RootPath}modules\{#MODULE_NAME}\functions\@char\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\@char\;Components: {#COMPONENT_CONTROL_SYSTEM};
Source: {#RootPath}modules\{#MODULE_NAME}\functions\@tf\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\@tf\;Components: {#COMPONENT_CONTROL_SYSTEM};
Source: {#RootPath}modules\{#MODULE_NAME}\functions\@ss\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\@ss\;Components: {#COMPONENT_CONTROL_SYSTEM};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.qch; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_CONTROL_SYSTEM} and {#COMPONENT_HELP_FILES} and {#COMPONENT_HELP_BROWSER};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_CONTROL_SYSTEM} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
