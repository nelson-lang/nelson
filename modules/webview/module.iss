;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "webview"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsWebview.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_WEBVIEW};
Source: {#RootPath}bin\{#BinPath}\libnlsWebview_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_WEBVIEW};
; WebView2 loader: resolved at runtime with LoadLibraryA("WebView2Loader.dll"),
; so it must ship next to the binaries. The system-wide WebView2 Runtime is not
; enough on its own; without this file the embedded WebView silently degrades to
; an app-mode browser window.
Source: {#RootPath}bin\{#BinPath}\WebView2Loader.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_WEBVIEW};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_WEBVIEW};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_WEBVIEW};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_WEBVIEW};
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\; Flags: recursesubdirs;Components: {#COMPONENT_WEBVIEW};
Source: {#RootPath}modules\{#MODULE_NAME}\frontend\examples\*; DestDir: {app}\modules\{#MODULE_NAME}\frontend\examples\; Excludes: "*.d.ts"; Flags: recursesubdirs;Components: {#COMPONENT_WEBVIEW};
Source: {#RootPath}modules\{#MODULE_NAME}\examples\*; DestDir: {app}\modules\{#MODULE_NAME}\examples\; Flags: recursesubdirs;Components: {#COMPONENT_WEBVIEW};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.nhz; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: ({#COMPONENT_WEBVIEW}) and {#COMPONENT_HELP_FILES};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_TESTS_MANAGER} and ({#COMPONENT_WEBVIEW}) and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.css; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_TESTS_MANAGER} and ({#COMPONENT_WEBVIEW}) and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.html; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_TESTS_MANAGER} and ({#COMPONENT_WEBVIEW}) and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.js; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_TESTS_MANAGER} and ({#COMPONENT_WEBVIEW}) and {#COMPONENT_UNIT_TESTS};
;==============================================================================
