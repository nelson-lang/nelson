;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "localization"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsLocalization.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_INTERNATIONALIZATION};
Source: {#RootPath}bin\{#BinPath}\libno-nlsLocalization.dll; DestDir: {app}\bin\{#BinPath}\;DestName: libnlsLocalization.dll;Components: not {#COMPONENT_INTERNATIONALIZATION};
Source: {#RootPath}bin\{#BinPath}\libnlsLocalization_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_INTERNATIONALIZATION};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_INTERNATIONALIZATION};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_INTERNATIONALIZATION};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_INTERNATIONALIZATION};
;==============================================================================
;Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\;Components: {#COMPONENT_INTERNATIONALIZATION};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.nhz; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_INTERNATIONALIZATION} and {#COMPONENT_HELP_FILES};
;==============================================================================
