;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "sio_client"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libno-nlsSio_client.dll; DestDir: {app}\bin\{#BinPath}\;DestName: libnlsSio_client.dll;Components: not {#COMPONENT_SIO_CLIENT};
Source: {#RootPath}bin\{#BinPath}\libnlsSio_client.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_SIO_CLIENT};
Source: {#RootPath}bin\{#BinPath}\libnlsSio_client_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_SIO_CLIENT};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_SIO_CLIENT};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_SIO_CLIENT};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_SIO_CLIENT};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\;Components: {#COMPONENT_SIO_CLIENT};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.qch; DestDir: {app}\modules\{#MODULE_NAME}\help\;Flags: recursesubdirs;Components: {#COMPONENT_HELP_FILES} and {#COMPONENT_SIO_CLIENT}; 
;==============================================================================
;Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
