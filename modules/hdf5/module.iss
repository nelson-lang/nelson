;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "hdf5"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsHdf5.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_HDF5};
Source: {#RootPath}bin\{#BinPath}\libnlsHdf5_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_HDF5};
Source: {#RootPath}bin\{#BinPath}\hdf5.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_HDF5};
Source: {#RootPath}bin\{#BinPath}\aec.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_HDF5};
Source: {#RootPath}bin\{#BinPath}\zlib1.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_HDF5};
Source: {#RootPath}bin\{#BinPath}\szip.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_HDF5};
Source: {#RootPath}bin\{#BinPath}\h5dump.exe; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_HDF5};
Source: {#RootPath}bin\{#BinPath}\h5ls.exe; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_HDF5};
#ifdef NELSON_WOA64
Source: {#RootPath}bin\{#BinPath}\hdf5_hl.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_HDF5};
Source: {#RootPath}bin\{#BinPath}\hdf5_tools.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_HDF5};
#endif
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_HDF5};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_HDF5};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_HDF5};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\;Components: {#COMPONENT_HDF5};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.nhz; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_HDF5} and {#COMPONENT_HELP_FILES};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_HDF5} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.ref; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_HDF5} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.nh5; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_HDF5} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\h5\*.h5; DestDir: {app}\modules\{#MODULE_NAME}\tests\h5\;Components: {#COMPONENT_HDF5} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
