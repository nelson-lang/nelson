;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
; web_engine is a shared engine library (webui + helpers), not a loaded module:
; its desktop builtins, functions, help and tests now live in the gui module.
#define MODULE_NAME "web_engine"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsWeb_engine.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_WEB_ENGINE};
; WebView2 loader: resolved at runtime with LoadLibraryA("WebView2Loader.dll"),
; so it must ship next to the binaries. The system-wide WebView2 Runtime is not
; enough on its own; without this file the embedded WebView silently degrades to
; an app-mode browser window.
Source: {#RootPath}bin\{#BinPath}\WebView2Loader.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_WEB_ENGINE};
;==============================================================================
; Shared example-gallery assets, consumed by the desktop and nflow front ends.
Source: {#RootPath}modules\{#MODULE_NAME}\frontend\examples\*; DestDir: {app}\modules\{#MODULE_NAME}\frontend\examples\; Excludes: "*.d.ts"; Flags: recursesubdirs;Components: {#COMPONENT_WEB_ENGINE};
;==============================================================================
