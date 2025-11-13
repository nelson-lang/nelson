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
Source: {#RootPath}bin\{#BinPath}\boost_chrono-{#BOOST_TARGET}.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
Source: {#RootPath}bin\{#BinPath}\boost_date_time-{#BOOST_TARGET}.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
Source: {#RootPath}bin\{#BinPath}\boost_filesystem-{#BOOST_TARGET}.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
Source: {#RootPath}bin\{#BinPath}\boost_iostreams-{#BOOST_TARGET}.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
Source: {#RootPath}bin\{#BinPath}\boost_program_options-{#BOOST_TARGET}.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
Source: {#RootPath}bin\{#BinPath}\boost_regex-{#BOOST_TARGET}.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
;Source: {#RootPath}bin\{#BinPath}\boost_system-{#BOOST_TARGET}.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
Source: {#RootPath}bin\{#BinPath}\boost_thread-{#BOOST_TARGET}.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
Source: {#RootPath}bin\{#BinPath}\boost_random-{#BOOST_TARGET}.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
Source: {#RootPath}bin\{#BinPath}\boost_zlib-{#BOOST_TARGET}.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
Source: {#RootPath}bin\{#BinPath}\boost_bzip2-{#BOOST_TARGET}.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
Source: {#RootPath}bin\{#BinPath}\boost_serialization-{#BOOST_TARGET}.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
Source: {#RootPath}bin\{#BinPath}\boost_process-{#BOOST_TARGET}.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
;==============================================================================
; vc runtime
#ifdef NELSON_X64
Source: {#RootPath}bin\{#BinPath}\vc_redist.x64.exe; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
Source: {#RootPath}bin\{#BinPath}\vcruntime140_1.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
#else
Source: {#RootPath}bin\{#BinPath}\vc_redist.x86.exe; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
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
; intel runtime
Source: {#RootPath}bin\{#BinPath}\libmmd.dll; DestDir: {app}\bin\{#BinPath}\ ; Components: {#COMPONENT_NELSON};
#ifdef ICC_COMPILER
Source: {#RootPath}bin\{#BinPath}\svml_dispmd.dll; DestDir: {app}\bin\{#BinPath}\ ; Components: {#COMPONENT_NELSON};
#endif
#ifdef ICX_COMPILER
Source: {#RootPath}bin\{#BinPath}\svml_dispmd.dll; DestDir: {app}\bin\{#BinPath}\ ; Components: {#COMPONENT_NELSON};
#endif
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\LICENSE_1_0.txt; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_NELSON};
;==============================================================================
; used by linear_algebra and slicot modules
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\mkl\libiomp5md.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_CPU_OPTIMIZATION}
Source: {#RootPath}bin\{#BinPath}\mkl\libnlsblaslapack.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_CPU_OPTIMIZATION}
Source: {#RootPath}bin\{#BinPath}\mkl\libnlsblaslapack.lib; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_CPU_OPTIMIZATION} and {#COMPONENT_DYNAMIC_LINK}
Source: {#RootPath}bin\{#BinPath}\mkl\mkl_license.txt; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_CPU_OPTIMIZATION}
;==============================================================================
; vml wrapper (simd)
Source: {#RootPath}bin\{#BinPath}\mkl\libnlsvml_mkl.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_CPU_OPTIMIZATION}
;==============================================================================
