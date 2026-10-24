;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "netcdf"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsNetcdf.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_NETCDF};
Source: {#RootPath}bin\{#BinPath}\libnlsNetcdf_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_NETCDF};
Source: {#RootPath}bin\{#BinPath}\netcdf.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_NETCDF};
Source: {#RootPath}bin\{#BinPath}\hdf5.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_NETCDF};
Source: {#RootPath}bin\{#BinPath}\hdf5_hl.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_NETCDF};
Source: {#RootPath}bin\{#BinPath}\aec.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_NETCDF};
Source: {#RootPath}bin\{#BinPath}\szip.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_NETCDF};
Source: {#RootPath}bin\{#BinPath}\z.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_NETCDF};
Source: {#RootPath}bin\{#BinPath}\libcurl.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_NETCDF};
Source: {#RootPath}bin\{#BinPath}\tinyxml2.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_NETCDF};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_NETCDF};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_NETCDF};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_NETCDF};
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\;Components: {#COMPONENT_NETCDF};
Source: {#RootPath}modules\{#MODULE_NAME}\functions\+netcdf\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\+netcdf\;Components: {#COMPONENT_NETCDF};
Source: {#RootPath}modules\{#MODULE_NAME}\examples\*; DestDir: {app}\modules\{#MODULE_NAME}\examples\; Flags: recursesubdirs;Components: {#COMPONENT_NETCDF};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.nhz; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_NETCDF} and {#COMPONENT_HELP_FILES};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_NETCDF} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.nc; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_NETCDF} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
