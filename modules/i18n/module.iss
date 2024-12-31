;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of the Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "i18n"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsI18n.lib; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_DYNAMIC_LINK};
Source: {#RootPath}modules\{#MODULE_NAME}\src\include\*.hpp; DestDir: {app}\modules\{#MODULE_NAME}\src\include\;Components: {#COMPONENT_DYNAMIC_LINK};
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsI18n.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_INTERNATIONALIZATION};
Source: {#RootPath}bin\{#BinPath}\libnlsI18n_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_INTERNATIONALIZATION};
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libno-nlsI18n.dll; DestDir: {app}\bin\{#BinPath}\;DestName:libnlsI18n.dll;Components: not {#COMPONENT_INTERNATIONALIZATION};
Source: {#RootPath}bin\{#BinPath}\libno-nlsI18n_builtin.dll; DestDir: {app}\bin\{#BinPath}\;DestName:libnlsI18n_builtin.dll;Components: not {#COMPONENT_INTERNATIONALIZATION};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\;
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.qch; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components:{#COMPONENT_HELP_FILES} and {#COMPONENT_HELP_BROWSER};
;==============================================================================
