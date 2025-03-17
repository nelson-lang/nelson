;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of the Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "graphics"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsGraphics.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GRAPHICS} and {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\libnlsGraphics_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GRAPHICS} and {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\gif.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GRAPHICS} and {#COMPONENT_GUI};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_GRAPHICS} and {#COMPONENT_GUI};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_GRAPHICS} and {#COMPONENT_GUI};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_GRAPHICS} and {#COMPONENT_GUI};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\resources\*.*; DestDir: {app}\modules\{#MODULE_NAME}\resources\;Components: {#COMPONENT_GRAPHICS} and {#COMPONENT_GUI};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\examples\movie\*.png; DestDir: {app}\modules\{#MODULE_NAME}\examples\movie;Flags: recursesubdirs;Components: {#COMPONENT_GRAPHICS} and {#COMPONENT_GUI};
Source: {#RootPath}modules\{#MODULE_NAME}\examples\*.m; DestDir: {app}\modules\{#MODULE_NAME}\examples\;Flags: recursesubdirs;Components: {#COMPONENT_GRAPHICS} and {#COMPONENT_GUI};
Source: {#RootPath}modules\{#MODULE_NAME}\examples\*.nh5; DestDir: {app}\modules\{#MODULE_NAME}\examples\;Flags: recursesubdirs;Components: {#COMPONENT_GRAPHICS} and {#COMPONENT_GUI};
Source: {#RootPath}modules\{#MODULE_NAME}\examples\*.md; DestDir: {app}\modules\{#MODULE_NAME}\examples\;Flags: recursesubdirs;Components: {#COMPONENT_GRAPHICS} and {#COMPONENT_GUI};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\;Components: {#COMPONENT_GRAPHICS} and {#COMPONENT_GUI};
Source: {#RootPath}modules\{#MODULE_NAME}\functions\colormaps\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\colormaps;Components: {#COMPONENT_GRAPHICS} and {#COMPONENT_GUI};
Source: {#RootPath}modules\{#MODULE_NAME}\functions\private\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\private;Components: {#COMPONENT_GRAPHICS} and {#COMPONENT_GUI};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.qch; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_GUI} and {#COMPONENT_GRAPHICS} and {#COMPONENT_HELP_FILES} and {#COMPONENT_HELP_BROWSER};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_GUI} and {#COMPONENT_GRAPHICS} and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\images\*.*; DestDir: {app}\modules\{#MODULE_NAME}\tests\images\; Flags: recursesubdirs;Components: {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_GUI} and {#COMPONENT_GRAPHICS} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
