;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of the Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU Lesser General Public
; License as published by the Free Software Foundation; either
; version 2.1 of the License, or (at your option) any later version.
;
; Alternatively, you can redistribute it and/or
; modify it under the terms of the GNU General Public License as
; published by the Free Software Foundation; either version 2 of
; the License, or (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU Lesser General Public License for more details.
;
; You should have received a copy of the GNU Lesser General Public
; License along with this program. If not, see <http://www.gnu.org/licenses/>.
; LICENCE_BLOCK_END
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\readme.txt; DestDir: {app}\bin\{#BinPath}\
;==============================================================================
; Boost 1.71
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
Source: {#RootPath}bin\{#BinPath}\boost_zlib-{#BOOST_TARGET}.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\boost_bzip2-{#BOOST_TARGET}.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\boost_serialization-{#BOOST_TARGET}.dll; DestDir: {app}\bin\{#BinPath}\;
;==============================================================================
; xml libraries
Source: {#RootPath}bin\{#BinPath}\zlib.dll; DestDir: {app}\bin\{#BinPath}\ 
Source: {#RootPath}bin\{#BinPath}\libiconv.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\libxml2.dll; DestDir: {app}\bin\{#BinPath}\
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
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\mkl\libiomp5md.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_CPU_OPTIMIZATION}
Source: {#RootPath}bin\{#BinPath}\mkl\libnlsblaslapack.dll; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_CPU_OPTIMIZATION}
Source: {#RootPath}bin\{#BinPath}\mkl\mkl_license.txt; DestDir: {app}\bin\{#BinPath}\; Components: {#COMPONENT_CPU_OPTIMIZATION}
;==============================================================================
