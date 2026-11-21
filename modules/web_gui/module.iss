;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "web_gui"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsWeb_gui.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_WEB_GUI};
;==============================================================================
; Frontend bundle (index.html + app.js built by vite from frontend/): served by
; nelson-webview, which resolves it from the module tree
; (<nelson modules dir>\web_gui\frontend\dist), same convention as nflow_gui.
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\frontend\dist\*; DestDir: {app}\modules\{#MODULE_NAME}\frontend\dist\; Flags: recursesubdirs;Components: {#COMPONENT_WEB_GUI};
; Client-only figure replayer bundle, inlined by saveas(fig,'f.html') into a
; self-contained interactive page. Resolved from the module tree at runtime.
Source: {#RootPath}modules\{#MODULE_NAME}\frontend\dist-replayer\*; DestDir: {app}\modules\{#MODULE_NAME}\frontend\dist-replayer\; Flags: recursesubdirs;Components: {#COMPONENT_WEB_GUI};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_WEB_GUI} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
