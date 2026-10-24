;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "render_web"
;==============================================================================
; RenderWeb is the headless (Qt-free) rendering host; it is only linked by the
; graphics modules, so it ships with the GRAPHICS component.
Source: {#RootPath}bin\{#BinPath}\libnlsRenderWeb.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GRAPHICS};
;==============================================================================
