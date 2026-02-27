;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
; Boost 1.89
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\boost_thread-{#BOOST_TARGET}.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
Source: {#RootPath}bin\{#BinPath}\boost_serialization-{#BOOST_TARGET}.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
;==============================================================================
; VC runtime
;==============================================================================
#if defined(NELSON_WOA64)
Source: {#RootPath}bin\{#BinPath}\vc_redist.arm64.exe; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
#elif defined(NELSON_X64)
Source: {#RootPath}bin\{#BinPath}\vc_redist.x64.exe; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
#else
Source: {#RootPath}bin\{#BinPath}\vc_redist.x86.exe; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
#endif
#if defined(NELSON_WOA64) || defined(NELSON_X64)
Source: {#RootPath}bin\{#BinPath}\vcruntime140_1.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
#endif
Source: {#RootPath}bin\{#BinPath}\msvcp140.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
Source: {#RootPath}bin\{#BinPath}\vcruntime140.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
Source: {#RootPath}bin\{#BinPath}\concrt140.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
Source: {#RootPath}bin\{#BinPath}\msvcp140_1.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
Source: {#RootPath}bin\{#BinPath}\msvcp140_2.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
Source: {#RootPath}bin\{#BinPath}\msvcp140_atomic_wait.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
Source: {#RootPath}bin\{#BinPath}\msvcp140_codecvt_ids.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
Source: {#RootPath}bin\{#BinPath}\vcamp140.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
Source: {#RootPath}bin\{#BinPath}\vccorlib140.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
Source: {#RootPath}bin\{#BinPath}\vcomp140.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
Source: {#RootPath}bin\{#BinPath}\vcruntime140_threads.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
;==============================================================================
; Intel runtime
;==============================================================================
#ifndef NELSON_WOA64
Source: {#RootPath}bin\{#BinPath}\libmmd.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
#endif
#if defined(ICC_COMPILER) || defined(ICX_COMPILER)
Source: {#RootPath}bin\{#BinPath}\svml_dispmd.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
#endif
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\LICENSE_1_0.txt; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
;==============================================================================
; BLAS/LAPACK libraries (used by linear_algebra and slicot modules)
;==============================================================================
#ifdef NELSON_WOA64
Source: {#RootPath}bin\{#BinPath}\libnlsblaslapack.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\libnlsblaslapack.lib; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_DYNAMIC_LINK};
#else
Source: {#RootPath}bin\{#BinPath}\mkl\libiomp5md.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_CPU_OPTIMIZATION};
Source: {#RootPath}bin\{#BinPath}\mkl\libnlsblaslapack.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_CPU_OPTIMIZATION};
Source: {#RootPath}bin\{#BinPath}\mkl\libnlsblaslapack.lib; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_CPU_OPTIMIZATION} and {#COMPONENT_DYNAMIC_LINK};
Source: {#RootPath}bin\{#BinPath}\mkl\mkl_license.txt; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_CPU_OPTIMIZATION};
; VML wrapper (SIMD)
Source: {#RootPath}bin\{#BinPath}\mkl\libnlsvml_mkl.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_CPU_OPTIMIZATION};
#endif
;==============================================================================
