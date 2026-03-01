;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
; Qt libraries
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\qt.conf; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
;==============================================================================
; Qt plugins and QML resources
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\plugins\*; DestDir: {app}\bin\{#BinPath}\plugins; Flags: recursesubdirs;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\qml\*; DestDir: {app}\bin\{#BinPath}\qml; Flags: recursesubdirs;Components: {#COMPONENT_GUI};
;==============================================================================
; Platform-specific graphics libraries (not available on WoA64)
;==============================================================================
#ifndef NELSON_WOA64
Source: {#RootPath}bin\{#BinPath}\d3dcompiler_*.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\opengl32sw.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
#endif
;==============================================================================
; Qt5-only non-Qt-prefixed libraries
;==============================================================================
#ifdef QT5_USED
Source: {#RootPath}bin\{#BinPath}\libEGL.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\libGLESv2.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
#endif
;==============================================================================
; All Qt DLLs (wildcard covers both Qt5 and Qt6 prefixed libraries)
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\Qt?*.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
;==============================================================================
