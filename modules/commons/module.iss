;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "commons"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsCommons.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\libnlsCommons.lib; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_DYNAMIC_LINK};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\src\include\Nelson_VERSION.h; DestDir: {app}\modules\{#MODULE_NAME}\src\include\;
Source: {#RootPath}modules\{#MODULE_NAME}\src\include\nlsBuildConfig.h; DestDir: {app}\modules\{#MODULE_NAME}\src\include\;
; recurse so the SDK third-party trees (nlohmann, fast_float) ship, needed to build native plugins
Source: {#RootPath}modules\{#MODULE_NAME}\src\include\*.h; DestDir: {app}\modules\{#MODULE_NAME}\src\include\; Flags: recursesubdirs;Components: {#COMPONENT_DYNAMIC_LINK};
Source: {#RootPath}modules\{#MODULE_NAME}\src\include\*.hpp; DestDir: {app}\modules\{#MODULE_NAME}\src\include\; Flags: recursesubdirs;Components: {#COMPONENT_DYNAMIC_LINK};
;==============================================================================
