;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "parquet"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsParquet.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_PARQUET};
Source: {#RootPath}bin\{#BinPath}\libnlsParquet_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_PARQUET};
Source: {#RootPath}bin\{#BinPath}\abseil_dll.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_PARQUET};
Source: {#RootPath}bin\{#BinPath}\arrow*.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_PARQUET};
Source: {#RootPath}bin\{#BinPath}\boost_*.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_PARQUET};
Source: {#RootPath}bin\{#BinPath}\brotli*.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_PARQUET};
Source: {#RootPath}bin\{#BinPath}\bz2.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_PARQUET};
Source: {#RootPath}bin\{#BinPath}\event*.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_PARQUET};
Source: {#RootPath}bin\{#BinPath}\gflags.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_PARQUET};
Source: {#RootPath}bin\{#BinPath}\legacy.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_PARQUET};
Source: {#RootPath}bin\{#BinPath}\libcrypto*.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_PARQUET};
Source: {#RootPath}bin\{#BinPath}\libssl*.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_PARQUET};
Source: {#RootPath}bin\{#BinPath}\lz4.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_PARQUET};
Source: {#RootPath}bin\{#BinPath}\mimalloc*.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_PARQUET};
Source: {#RootPath}bin\{#BinPath}\parquet.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_PARQUET};
Source: {#RootPath}bin\{#BinPath}\re2.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_PARQUET};
Source: {#RootPath}bin\{#BinPath}\snappy.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_PARQUET};
Source: {#RootPath}bin\{#BinPath}\utf8proc.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_PARQUET};
Source: {#RootPath}bin\{#BinPath}\z.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_PARQUET};
Source: {#RootPath}bin\{#BinPath}\zstd.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_PARQUET};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_PARQUET};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_PARQUET};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_PARQUET};
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\;Components: {#COMPONENT_PARQUET};
Source: {#RootPath}modules\{#MODULE_NAME}\functions\+nelson\*; DestDir: {app}\modules\{#MODULE_NAME}\functions\+nelson\; Flags: recursesubdirs;Components: {#COMPONENT_PARQUET};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\examples\*; DestDir: {app}\modules\{#MODULE_NAME}\examples\; Flags: recursesubdirs;Components: {#COMPONENT_PARQUET};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.nhz; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_PARQUET} and {#COMPONENT_HELP_FILES};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_PARQUET} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
