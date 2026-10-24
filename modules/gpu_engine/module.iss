;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "gpu_engine"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsGpu_engine.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GPU_ENGINE};
Source: {#RootPath}bin\{#BinPath}\libnlsGpu_engine_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GPU_ENGINE};
Source: {#RootPath}bin\{#BinPath}\webgpu_dawn.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GPU_ENGINE};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_GPU_ENGINE};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_GPU_ENGINE};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_GPU_ENGINE};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\functions\@gpuArray\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\@gpuArray\;Components: {#COMPONENT_GPU_ENGINE};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\examples\*; DestDir: {app}\modules\{#MODULE_NAME}\examples\; Flags: recursesubdirs;Components: {#COMPONENT_GPU_ENGINE};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.nhz; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_GPU_ENGINE} and {#COMPONENT_HELP_FILES};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_GPU_ENGINE} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
