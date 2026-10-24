;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "font_metrics"
;==============================================================================
; FontMetrics is only linked by the graphics modules and RenderWeb, so it
; ships with the GRAPHICS component.
Source: {#RootPath}bin\{#BinPath}\libnlsFontMetrics.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GRAPHICS};
;==============================================================================
