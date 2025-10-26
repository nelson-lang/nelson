;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "main"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\nelson.bat; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\NelSon-adv-cli.exe; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\NelSon-cli.exe; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\NelSon-gui.exe; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\NelSon-sio-cli.exe; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_SIO_CLIENT};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.nhz; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_HELP_FILES};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\en_US\xml\summary_header.xml; DestDir: {app}\modules\{#MODULE_NAME}\help\en_US\xml; Flags: recursesubdirs;Components: {#COMPONENT_HELP_FILES};
Source: {#RootPath}modules\{#MODULE_NAME}\help\fr_FR\xml\summary_header.xml; DestDir: {app}\modules\{#MODULE_NAME}\help\fr_FR\xml; Flags: recursesubdirs;Components: {#COMPONENT_HELP_FILES};
;==============================================================================
