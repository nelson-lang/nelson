;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "mpi"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsMpi.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_MPI};
Source: {#RootPath}bin\{#BinPath}\libnlsMpi_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_MPI};
Source: {#RootPath}bin\{#BinPath}\MSMpiSetup.exe; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_MPI};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_MPI};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_MPI};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_MPI};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\examples\*.m; DestDir: {app}\modules\{#MODULE_NAME}\examples\; Flags: recursesubdirs;Components: {#COMPONENT_MPI};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\;Components: {#COMPONENT_MPI};
Source: {#RootPath}modules\{#MODULE_NAME}\functions\@MPI_Comm\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\@MPI_Comm\;Components: {#COMPONENT_MPI};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.qch; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_MPI} and {#COMPONENT_HELP_FILES} and {#COMPONENT_HELP_BROWSER};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_MPI} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
