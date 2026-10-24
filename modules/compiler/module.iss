;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "compiler"
Source: {#RootPath}bin\{#BinPath}\libnlsCompiler.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_COMPILER};
Source: {#RootPath}bin\{#BinPath}\libnlsCompiler_builtin.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_COMPILER};
Source: {#RootPath}bin\{#BinPath}\nelsonc.exe; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_COMPILER};
Source: {#RootPath}bin\{#BinPath}\nelson-app.exe; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_COMPILER};
Source: {#RootPath}bin\{#BinPath}\nelson-app-windowed.exe; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_COMPILER};
Source: {#RootPath}bin\{#BinPath}\nelson-installer.exe; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_COMPILER};
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\; Components: {#COMPONENT_COMPILER};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\*.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\; Components: {#COMPONENT_COMPILER};
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\; Components: {#COMPONENT_COMPILER};
Source: {#RootPath}modules\{#MODULE_NAME}\functions\+nelson\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\+nelson\; Flags: recursesubdirs; Components: {#COMPONENT_COMPILER};
Source: {#RootPath}modules\{#MODULE_NAME}\functions\+compiler\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\+compiler\; Flags: recursesubdirs; Components: {#COMPONENT_COMPILER};
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.nhz; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs; Components: {#COMPONENT_COMPILER} and {#COMPONENT_HELP_FILES};
Source: {#RootPath}modules\{#MODULE_NAME}\examples\*; DestDir: {app}\modules\{#MODULE_NAME}\examples\; Flags: recursesubdirs; Components: {#COMPONENT_COMPILER};
Source: {#RootPath}modules\{#MODULE_NAME}\resources\*.iss; DestDir: {app}\modules\{#MODULE_NAME}\resources\; Components: {#COMPONENT_COMPILER};
Source: {#RootPath}modules\{#MODULE_NAME}\resources\editor\*.png; DestDir: {app}\modules\{#MODULE_NAME}\resources\editor\; Components: {#COMPONENT_COMPILER};
Source: {#RootPath}modules\{#MODULE_NAME}\resources\linux\*.sh; DestDir: {app}\modules\{#MODULE_NAME}\resources\linux\; Components: {#COMPONENT_COMPILER};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs; Components: {#COMPONENT_COMPILER} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
