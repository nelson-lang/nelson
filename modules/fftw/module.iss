;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of the Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "fftw"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\mkl\libfftw3f-3.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_CPU_OPTIMIZATION} and {#COMPONENT_FFTW} 
Source: {#RootPath}bin\{#BinPath}\mkl\libfftw3-3.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_CPU_OPTIMIZATION} and {#COMPONENT_FFTW}
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsFftw.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_FFTW}
Source: {#RootPath}bin\{#BinPath}\libnlsFftw_builtin.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_FFTW}
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\; Components: {#COMPONENT_FFTW}
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\; Components: {#COMPONENT_FFTW}
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\; Components: {#COMPONENT_FFTW}
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\; Components: {#COMPONENT_FFTW}
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.qch; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs; Components: {#COMPONENT_FFTW} and {#COMPONENT_HELP_FILES} and {#COMPONENT_HELP_BROWSER};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs; Components: {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_FFTW} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
