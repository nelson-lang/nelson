;==============================================================================
; Copyright (c) 2016-2018 Allan CORNET (Nelson)
;==============================================================================
; LICENCE_BLOCK_BEGIN
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 2 of the License, or
; (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
; LICENCE_BLOCK_END
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\readme.txt; DestDir: {app}\bin\{#BinPath}\
;==============================================================================
; Boost 1.67
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\boost_chrono-{#BOOST_TARGET}.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\boost_date_time-{#BOOST_TARGET}.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\boost_filesystem-{#BOOST_TARGET}.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\boost_iostreams-{#BOOST_TARGET}.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\boost_locale-{#BOOST_TARGET}.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\boost_program_options-{#BOOST_TARGET}.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\boost_regex-{#BOOST_TARGET}.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\boost_system-{#BOOST_TARGET}.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\boost_thread-{#BOOST_TARGET}.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\boost_random-{#BOOST_TARGET}.dll; DestDir: {app}\bin\{#BinPath}\;
;==============================================================================
; xml libraries
Source: {#RootPath}bin\{#BinPath}\zlib1.dll; DestDir: {app}\bin\{#BinPath}\ 
#ifdef NELSON_X64
Source: {#RootPath}bin\{#BinPath}\libiconv-2.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\libxml2-2.dll; DestDir: {app}\bin\{#BinPath}\
#else
Source: {#RootPath}bin\{#BinPath}\libiconv.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\libxml2.dll; DestDir: {app}\bin\{#BinPath}\
#endif
;==============================================================================
; vc & intel runtime
#ifdef NELSON_X64
Source: {#RootPath}bin\{#BinPath}\vcredist_x64.exe; DestDir: {app}\bin\{#BinPath}\
#else
Source: {#RootPath}bin\{#BinPath}\vcredist_x86.exe; DestDir: {app}\bin\{#BinPath}\
#endif
Source: {#RootPath}bin\{#BinPath}\msvcp140.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\vcruntime140.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\libmmd.dll; DestDir: {app}\bin\{#BinPath}\ 
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\LICENSE_1_0.txt; DestDir: {app}\bin\{#BinPath}\;
;==============================================================================
; used by linear_algebra and slicot modules
#ifdef NELSON_X64
Source: {#RootPath}bin\{#BinPath}\openblas\libgcc_s_seh-1.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_DEFAULT_CPU_LIBRARY}
Source: {#RootPath}bin\{#BinPath}\openblas\libgfortran-3.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_DEFAULT_CPU_LIBRARY}
Source: {#RootPath}bin\{#BinPath}\openblas\libquadmath-0.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_DEFAULT_CPU_LIBRARY}
#endif
Source: {#RootPath}bin\{#BinPath}\openblas\libnlsblaslapack.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_DEFAULT_CPU_LIBRARY}
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\mkl\libiomp5md.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_MKL_CPU_LIBRARY}
Source: {#RootPath}bin\{#BinPath}\mkl\libnlsblaslapack.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_MKL_CPU_LIBRARY}
Source: {#RootPath}bin\{#BinPath}\mkl\mkl_avx.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_MKL_CPU_LIBRARY}
Source: {#RootPath}bin\{#BinPath}\mkl\mkl_avx2.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_MKL_CPU_LIBRARY}
Source: {#RootPath}bin\{#BinPath}\mkl\mkl_avx512.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_MKL_CPU_LIBRARY}
Source: {#RootPath}bin\{#BinPath}\mkl\mkl_core.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_MKL_CPU_LIBRARY}
Source: {#RootPath}bin\{#BinPath}\mkl\mkl_intel_thread.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_MKL_CPU_LIBRARY}
Source: {#RootPath}bin\{#BinPath}\mkl\mkl_license.txt; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_MKL_CPU_LIBRARY}
Source: {#RootPath}bin\{#BinPath}\mkl\mkl_rt.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_MKL_CPU_LIBRARY}
#ifdef NELSON_X64
Source: {#RootPath}bin\{#BinPath}\mkl\mkl_avx512_mic.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_MKL_CPU_LIBRARY}
Source: {#RootPath}bin\{#BinPath}\mkl\mkl_mc.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_MKL_CPU_LIBRARY}
Source: {#RootPath}bin\{#BinPath}\mkl\mkl_mc3.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_MKL_CPU_LIBRARY}
#else
Source: {#RootPath}bin\{#BinPath}\mkl\mkl_p4.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_MKL_CPU_LIBRARY}
Source: {#RootPath}bin\{#BinPath}\mkl\mkl_p4m.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_MKL_CPU_LIBRARY}
Source: {#RootPath}bin\{#BinPath}\mkl\mkl_p4m3.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_MKL_CPU_LIBRARY}
#endif
;==============================================================================
