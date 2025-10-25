;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "f2c"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsF2C.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_F2C};
Source: {#RootPath}bin\{#BinPath}\libnlsF2C.lib; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_F2C} and {#COMPONENT_DYNAMIC_LINK};
Source: {#RootPath}bin\{#BinPath}\nelson_f2c.exe; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_F2C};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\src\include\nelson_f2c.h; DestDir: {app}\modules\{#MODULE_NAME}\src\include\;Components: {#COMPONENT_F2C} and {#COMPONENT_DYNAMIC_LINK};
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_F2C};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_F2C};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_F2C};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\;Components: {#COMPONENT_F2C};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.nhz; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_F2C} and {#COMPONENT_HELP_FILES};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_F2C} and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.f; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_F2C} and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.c; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_F2C} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
