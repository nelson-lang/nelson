;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "nelson_manager"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsNelson_manager.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\libnlsNelson_manager.lib; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_DYNAMIC_LINK};
Source: {#RootPath}modules\{#MODULE_NAME}\src\include\*.h; DestDir: {app}\modules\{#MODULE_NAME}\src\include\;Components: {#COMPONENT_DYNAMIC_LINK};
Source: {#RootPath}modules\{#MODULE_NAME}\src\include\*.hpp; DestDir: {app}\modules\{#MODULE_NAME}\src\include\;Components: {#COMPONENT_DYNAMIC_LINK};
