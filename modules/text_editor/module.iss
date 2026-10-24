;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "text_editor"
;==============================================================================
; The reduced no- stub only satisfies the static import from graphics_qt (GUI);
; a CLI-only install has no importer, so it ships only when the GUI is present.
Source: {#RootPath}bin\{#BinPath}\libno-nlsText_editor.dll; DestDir: {app}\bin\{#BinPath}\;DestName: libnlsText_editor.dll;Components: {#COMPONENT_GUI} and not {#COMPONENT_TEXT_EDITOR};
Source: {#RootPath}bin\{#BinPath}\libnlsText_editor.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_TEXT_EDITOR} and {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\libnlsText_editor_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_TEXT_EDITOR} and {#COMPONENT_GUI};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_TEXT_EDITOR} and {#COMPONENT_GUI};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_TEXT_EDITOR} and {#COMPONENT_GUI};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_TEXT_EDITOR} and {#COMPONENT_GUI};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\;Components: {#COMPONENT_TEXT_EDITOR} and {#COMPONENT_GUI};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\resources\*.*; DestDir: {app}\modules\{#MODULE_NAME}\resources\;Components: {#COMPONENT_TEXT_EDITOR} and {#COMPONENT_GUI};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.nhz; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_GUI} and {#COMPONENT_TEXT_EDITOR} and {#COMPONENT_HELP_FILES};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_GUI} and {#COMPONENT_TEXT_EDITOR} and {#COMPONENT_UNIT_TESTS};
;==============================================================================

