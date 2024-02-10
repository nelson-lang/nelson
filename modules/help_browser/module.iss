;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of the Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "help_browser"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libno-nlsHelp_browser.dll; DestDir: {app}\bin\{#BinPath}\;DestName: libnlsHelp_browser.dll;Components: not {#COMPONENT_HELP_BROWSER};
Source: {#RootPath}bin\{#BinPath}\libnlsHelp_browser.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI} and {#COMPONENT_HELP_BROWSER};
Source: {#RootPath}bin\{#BinPath}\libnlsHelp_browser_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI} and {#COMPONENT_HELP_BROWSER};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_GUI} and {#COMPONENT_HELP_BROWSER};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_GUI} and {#COMPONENT_HELP_BROWSER};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_GUI} and {#COMPONENT_HELP_BROWSER};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\;Components: {#COMPONENT_GUI} and {#COMPONENT_HELP_BROWSER};
Source: {#RootPath}modules\{#MODULE_NAME}\functions\private\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\private\;Components: {#COMPONENT_GUI} and {#COMPONENT_HELP_BROWSER};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.qch; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_GUI} and {#COMPONENT_HELP_FILES} and {#COMPONENT_HELP_BROWSER};
;==============================================================================
