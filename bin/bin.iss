;==============================================================================
; Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
; Boost 1.61
;==============================================================================
#ifndef NELSON_DEBUG
Source: {#RootPath}bin\{#BinPath}\boost_chrono-vc141-mt-1_64.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\boost_date_time-vc141-mt-1_64.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\boost_filesystem-vc141-mt-1_64.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\boost_iostreams-vc141-mt-1_64.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\boost_locale-vc141-mt-1_64.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\boost_program_options-vc141-mt-1_64.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\boost_regex-vc141-mt-1_64.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\boost_system-vc141-mt-1_64.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\boost_thread-vc141-mt-1_64.dll; DestDir: {app}\bin\{#BinPath}\;
;==============================================================================
#else
Source: {#RootPath}bin\{#BinPath}\boost_chrono-vc141-mt-gd-1_64.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\boost_date_time-vc141-mt-gd-1_64.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\boost_filesystem-vc141-mt-gd-1_64.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\boost_iostreams-vc141-mt-gd-1_64.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\boost_locale-vc141-mt-gd-1_64.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\boost_program_options-vc141-mt-gd-1_64.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\boost_regex-vc141-mt-gd-1_64.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\boost_system-vc141-mt-gd-1_64.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\boost_thread-vc141-mt-gd-1_64.dll; DestDir: {app}\bin\{#BinPath}\;
#endif
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
Source: {#RootPath}bin\{#BinPath}\vc_redist.x64.exe; DestDir: {app}\bin\{#BinPath}\
#else
Source: {#RootPath}bin\{#BinPath}\vc_redist.x86.exe; DestDir: {app}\bin\{#BinPath}\
#endif
Source: {#RootPath}bin\{#BinPath}\msvcp140.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\vcruntime140.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\libmmd.dll; DestDir: {app}\bin\{#BinPath}\ 
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\LICENSE_1_0.txt; DestDir: {app}\bin\{#BinPath}\;
;==============================================================================
