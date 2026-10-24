;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "preferences"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsPreferences.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_HDF5};
; The full preferences builtin statically links the Qt-based gui library for the
; preference dialogs (uigetpref/uisetpref/preferences), so it ships only when the
; GUI is installed. A GUI-less install gets the reduced no- builtin (renamed),
; which keeps getpref/setpref/ispref/rmpref and reports the dialogs as
; unavailable, mirroring the headless CMake build.
Source: {#RootPath}bin\{#BinPath}\libnlsPreferences_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_HDF5} and {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\libno-nlsPreferences_builtin.dll; DestDir: {app}\bin\{#BinPath}\;DestName: libnlsPreferences_builtin.dll;Components: {#COMPONENT_HDF5} and not {#COMPONENT_GUI};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_HDF5};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_HDF5};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_HDF5};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_HDF5} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
