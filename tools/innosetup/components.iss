;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
; Components are grouped under category parents (grp_*) so the Select
; Components page is a readable, collapsible tree. The grp_* names are internal
; only; the visible label is the category CustomMessage. Child component names
; are "grp_<category>\<NAME>" via the COMPONENT_* defines, so every reference
; through those defines keeps working unchanged. Parents must be declared
; before their children.
;==============================================================================
; Core component (always installed)
;==============================================================================
#define COMPONENT_NELSON 'Nelson'
Name: {#COMPONENT_NELSON}; Description: Nelson {#APPLICATION_VERSION}; Types: full custom minimal; Flags: fixed;
;==============================================================================
; CPU optimization (not available on WoA64)
;==============================================================================
#define COMPONENT_CPU_OPTIMIZATION 'CPU_OPTIMIZATION'
#ifndef NELSON_WOA64
Name: {#COMPONENT_CPU_OPTIMIZATION}; Description:{cm:CPU_OPTIMIZATION_FOR_NELSON}; Types: full custom minimal; Flags: fixed;
#endif
;==============================================================================
; Math and numerical modules
;==============================================================================
#define COMPONENT_SLICOT 'grp_math\SLICOT'
#define COMPONENT_FFTW 'grp_math\FFTW'
#define COMPONENT_SIGNAL_PROCESSING 'grp_math\SIGNAL_PROCESSING'
#define COMPONENT_DATA_ANALYSIS 'grp_math\DATA_ANALYSIS'
#define COMPONENT_CATEGORICAL 'grp_math\CATEGORICAL'
#define COMPONENT_ODE_SOLVERS 'grp_math\ODE_SOLVERS'
#define COMPONENT_CONTROL_SYSTEM 'grp_math\CONTROL_SYSTEM'
#define COMPONENT_TRIGONOMETRIC_FUNCTIONS 'grp_math\TRIGONOMETRIC_FUNCTIONS'
#define COMPONENT_SPECIAL_FUNCTIONS 'grp_math\SPECIAL_FUNCTIONS'
#define COMPONENT_STATISTICS 'grp_math\STATISTICS'
#define COMPONENT_POLYNOMIAL_FUNCTIONS 'grp_math\POLYNOMIAL_FUNCTIONS'
#define COMPONENT_RANDOM 'grp_math\RANDOM'
#define COMPONENT_OPTIMIZATION 'grp_math\OPTIMIZATION'
#define COMPONENT_GEOMETRY 'grp_math\GEOMETRY'
Name: grp_math; Description:{cm:CATEGORY_MATH}; Types: full custom;
Name: {#COMPONENT_SLICOT}; Description:{cm:SLICOT_MODULE}; Types: full custom;
Name: {#COMPONENT_FFTW}; Description:{cm:FFTW_MODULE}; Types: full custom;
Name: {#COMPONENT_SIGNAL_PROCESSING}; Description:{cm:SIGNAL_PROCESSING_MODULE}; Types: full custom;
Name: {#COMPONENT_DATA_ANALYSIS}; Description:{cm:DATA_ANALYSIS_MODULE}; Types: full custom;
Name: {#COMPONENT_CATEGORICAL}; Description:{cm:CATEGORICAL_MODULE}; Types: full custom;
Name: {#COMPONENT_ODE_SOLVERS}; Description:{cm:ODE_SOLVERS_MODULE}; Types: full custom;
Name: {#COMPONENT_CONTROL_SYSTEM}; Description:{cm:CONTROL_SYSTEM_MODULE}; Types: full custom;
Name: {#COMPONENT_TRIGONOMETRIC_FUNCTIONS}; Description:{cm:TRIGONOMETRIC_FUNCTIONS_MODULE}; Types: full custom;
Name: {#COMPONENT_SPECIAL_FUNCTIONS}; Description:{cm:SPECIAL_FUNCTIONS_MODULE}; Types: full custom;
Name: {#COMPONENT_STATISTICS}; Description:{cm:STATISTICS_MODULE}; Types: full custom;
Name: {#COMPONENT_POLYNOMIAL_FUNCTIONS}; Description:{cm:POLYNOMIAL_FUNCTIONS_MODULE}; Types: full custom;
Name: {#COMPONENT_RANDOM}; Description:{cm:RANDOM_MODULE}; Types: full custom;
Name: {#COMPONENT_OPTIMIZATION}; Description:{cm:OPTIMIZATION_MODULE}; Types: full custom;
Name: {#COMPONENT_GEOMETRY}; Description:{cm:GEOMETRY_MODULE}; Types: full custom;
;==============================================================================
; Parallel and distributed computing
;==============================================================================
#define COMPONENT_MPI 'grp_parallel\MPI'
#define COMPONENT_PARALLEL 'grp_parallel\PARALLEL'
Name: grp_parallel; Description:{cm:CATEGORY_PARALLEL}; Types: full custom;
; MPI is not available on Windows ARM64 (no MS-MPI); do not offer the component there.
; It also requires the system-wide MS-MPI runtime, installed by MSMpiSetup.exe
; which needs administrative rights, so only offer it for an all-users install.
; When it is not selected (per-user install), updateModulesList() disables the
; mpi module in modules.m, so no files ship and the module is not loaded.
#ifndef NELSON_WOA64
Name: {#COMPONENT_MPI}; Description:{cm:MPI_MODULE}; Types: full custom; Check: IsAdminInstallMode;
#endif
Name: {#COMPONENT_PARALLEL}; Description:{cm:PARALLEL_MODULE}; Types: full custom;
;==============================================================================
; GUI, graphics, and editor modules
;==============================================================================
#define COMPONENT_GUI 'grp_ui\GUI'
#define COMPONENT_WEBVIEW 'grp_ui\WEBVIEW'
#define COMPONENT_WEB_GUI 'grp_ui\WEB_GUI'
#define COMPONENT_NFLOW 'grp_ui\NFLOW'
#define COMPONENT_QML_ENGINE 'grp_ui\QML_ENGINE'
#define COMPONENT_GRAPHICS 'grp_ui\GRAPHICS'
#define COMPONENT_IMAGE_PROCESSING 'grp_ui\IMAGE_PROCESSING'
#define COMPONENT_TEXT_EDITOR 'grp_ui\TEXT_EDITOR'
Name: grp_ui; Description:{cm:CATEGORY_GUI}; Types: full custom;
Name: {#COMPONENT_GUI}; Description:{cm:GUI_MODULE}; Types: full custom;
Name: {#COMPONENT_WEBVIEW}; Description:{cm:WEBVIEW_MODULE}; Types: full custom;
Name: {#COMPONENT_WEB_GUI}; Description:{cm:WEB_GUI_MODULE}; Types: full custom;
Name: {#COMPONENT_NFLOW}; Description:{cm:NFLOW_MODULE}; Types: full custom;
Name: {#COMPONENT_QML_ENGINE}; Description:{cm:QML_ENGINE_MODULE}; Types: full custom;
Name: {#COMPONENT_GRAPHICS}; Description:{cm:GRAPHICS_MODULE}; Types: full custom;
Name: {#COMPONENT_IMAGE_PROCESSING}; Description:{cm:IMAGE_PROCESSING_MODULE}; Types: full custom;
Name: {#COMPONENT_TEXT_EDITOR}; Description:{cm:TEXT_EDITOR_MODULE}; Types: full custom;
;==============================================================================
; Data I/O modules
;==============================================================================
#define COMPONENT_HDF5 'grp_io\HDF5'
#define COMPONENT_NETCDF 'grp_io\NETCDF'
#define COMPONENT_MATIO 'grp_io\MATIO'
#define COMPONENT_JSON 'grp_io\JSON'
#define COMPONENT_XML 'grp_io\XML'
#define COMPONENT_SPREADSHEET 'grp_io\SPREADSHEET'
#define COMPONENT_PARQUET 'grp_io\PARQUET'
Name: grp_io; Description:{cm:CATEGORY_IO}; Types: full custom;
Name: {#COMPONENT_HDF5}; Description:{cm:HDF5_MODULE}; Types: full custom;
Name: {#COMPONENT_NETCDF}; Description:{cm:NETCDF_MODULE}; Types: full custom;
Name: {#COMPONENT_MATIO}; Description:{cm:MATIO_MODULE}; Types: full custom;
Name: {#COMPONENT_JSON}; Description:{cm:JSON_MODULE}; Types: full custom;
Name: {#COMPONENT_XML}; Description:{cm:XML_MODULE}; Types: full custom;
Name: {#COMPONENT_SPREADSHEET}; Description:{cm:SPREADSHEET_MODULE}; Types: full custom;
Name: {#COMPONENT_PARQUET}; Description:{cm:PARQUET_MODULE}; Types: full custom;
;==============================================================================
; Communication, networking, and interop modules
;==============================================================================
#define COMPONENT_IPC 'grp_interop\IPC'
#define COMPONENT_WEBTOOLS 'grp_interop\WEBTOOLS'
#define COMPONENT_AI 'grp_interop\AI'
Name: grp_interop; Description:{cm:CATEGORY_INTEROP}; Types: full custom;
Name: {#COMPONENT_IPC}; Description:{cm:IPC_MODULE}; Types: full custom;
Name: {#COMPONENT_WEBTOOLS}; Description:{cm:WEBTOOLS_MODULE}; Types: full custom;
Name: {#COMPONENT_AI}; Description:{cm:AI_MODULE}; Types: full custom;
;==============================================================================
; Utility, help, and miscellaneous modules
;==============================================================================
#define COMPONENT_AUDIO 'grp_util\AUDIO'
#define COMPONENT_VALIDATORS 'grp_util\VALIDATORS'
#define COMPONENT_INTERNATIONALIZATION 'grp_util\INTERNATIONALIZATION'
#define COMPONENT_HELP_TOOLS 'grp_util\HELP_TOOLS'
#define COMPONENT_HELP_FILES 'grp_util\HELP_FILES'
#define COMPONENT_FILE_ARCHIVER 'grp_util\FILE_ARCHIVER'
#define COMPONENT_TEXT_COMPLETION 'grp_util\TEXT_COMPLETION'
Name: grp_util; Description:{cm:CATEGORY_UTIL}; Types: full custom;
Name: {#COMPONENT_AUDIO}; Description:{cm:AUDIO_MODULE}; Types: full custom;
Name: {#COMPONENT_VALIDATORS}; Description:{cm:VALIDATORS_MODULE}; Types: full custom;
Name: {#COMPONENT_INTERNATIONALIZATION}; Description:{cm:INTERNATIONALIZATION}; Types: full custom;
Name: {#COMPONENT_HELP_TOOLS}; Description:{cm:HELP_TOOLS_MODULE}; Types: full custom;
Name: {#COMPONENT_HELP_FILES}; Description:{cm:HELP_FILES}; Types: full custom;
Name: {#COMPONENT_FILE_ARCHIVER}; Description:{cm:FILE_ARCHIVER_MODULE}; Types: full custom;
Name: {#COMPONENT_TEXT_COMPLETION}; Description:{cm:TEXT_COMPLETION_MODULE}; Types: full custom;
;==============================================================================
; Development and testing modules
;==============================================================================
#define COMPONENT_TESTS_MANAGER 'grp_dev\TESTS_MANAGER'
#define COMPONENT_UNIT_TESTS 'grp_dev\UNIT_TESTS'
#define COMPONENT_DYNAMIC_LINK 'grp_dev\DYNAMIC_LINK'
#define COMPONENT_MEX 'grp_dev\MEX'
#define COMPONENT_F2C 'grp_dev\F2C'
#define COMPONENT_MODULE_SKELETON 'grp_dev\MODULE_SKELETON'
#define COMPONENT_ASSERT_FUNCTIONS 'grp_dev\ASSERT_FUNCTIONS'
Name: grp_dev; Description:{cm:CATEGORY_DEV}; Types: full custom;
Name: {#COMPONENT_TESTS_MANAGER}; Description:{cm:TESTS_MANAGER_MODULE}; Types: full custom;
Name: {#COMPONENT_UNIT_TESTS}; Description:{cm:UNIT_TESTS}; Types: full custom;
Name: {#COMPONENT_DYNAMIC_LINK}; Description:{cm:DYNAMIC_LINK_MODULE}; Types: full custom;
Name: {#COMPONENT_MEX}; Description:{cm:MEX_MODULE}; Types: full custom;
Name: {#COMPONENT_F2C}; Description:{cm:F2C_MODULE}; Types: full custom;
Name: {#COMPONENT_MODULE_SKELETON}; Description:{cm:MODULE_SKELETON}; Types: full custom;
Name: {#COMPONENT_ASSERT_FUNCTIONS}; Description:{cm:ASSERT_FUNCTIONS_MODULE}; Types: full custom;
#define COMPONENT_COMPILER 'grp_dev\COMPILER'
Name: {#COMPONENT_COMPILER}; Description:{cm:COMPILER_MODULE}; Types: full custom;
;==============================================================================
; External engine modules
;==============================================================================
#define COMPONENT_PYTHON_ENGINE 'grp_engines\PYTHON_ENGINE'
#define COMPONENT_PYTHON_RUNTIME 'grp_engines\PYTHON_ENGINE\PYTHON_RUNTIME'
Name: grp_engines; Description:{cm:CATEGORY_ENGINES}; Types: full custom;
Name: {#COMPONENT_PYTHON_ENGINE}; Description:{cm:PYTHON_ENGINE_MODULE}; Types: full custom;
Name: {#COMPONENT_PYTHON_RUNTIME}; Description:{cm:PYTHON_RUNTIME}; Types: full custom;
#ifdef WITH_JULIA_ENGINE
#define COMPONENT_JULIA_ENGINE 'grp_engines\JULIA_ENGINE'
Name: {#COMPONENT_JULIA_ENGINE}; Description:{cm:JULIA_ENGINE_MODULE}; Types: full custom;
#endif
#ifdef WITH_GPU_ENGINE
#define COMPONENT_GPU_ENGINE 'grp_engines\GPU_ENGINE'
Name: {#COMPONENT_GPU_ENGINE}; Description:{cm:GPU_ENGINE_MODULE}; Types: full custom;
#endif
;==============================================================================
