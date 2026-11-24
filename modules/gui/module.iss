;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "gui"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsGui.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_WEB_GUI};
Source: {#RootPath}bin\{#BinPath}\libnlsGui_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_WEB_GUI};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_WEB_GUI};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_WEB_GUI};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_WEB_GUI};
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\; Flags: recursesubdirs;Components: {#COMPONENT_WEB_GUI};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.nhz; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: ({#COMPONENT_WEB_GUI}) and {#COMPONENT_HELP_FILES};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_TESTS_MANAGER} and ({#COMPONENT_WEB_GUI}) and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.css; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_TESTS_MANAGER} and ({#COMPONENT_WEB_GUI}) and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.html; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_TESTS_MANAGER} and ({#COMPONENT_WEB_GUI}) and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.js; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_TESTS_MANAGER} and ({#COMPONENT_WEB_GUI}) and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.xml; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_TESTS_MANAGER} and ({#COMPONENT_WEB_GUI}) and {#COMPONENT_UNIT_TESTS};
;==============================================================================
; Frontend bundle (index.html + app.js built by vite from src/ts/frontend/):
; served by the desktop, which resolves it from the module tree
; (<nelson modules dir>\gui\dist), same convention as nflow_gui.
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\dist\*; DestDir: {app}\modules\{#MODULE_NAME}\dist\; Flags: recursesubdirs;Components: {#COMPONENT_WEB_GUI};
; Client-only figure replayer bundle, inlined by saveas(fig,'f.html').
Source: {#RootPath}modules\{#MODULE_NAME}\dist-replayer\*; DestDir: {app}\modules\{#MODULE_NAME}\dist-replayer\; Flags: recursesubdirs;Components: {#COMPONENT_WEB_GUI};
;==============================================================================
