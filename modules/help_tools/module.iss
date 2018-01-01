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
#define MODULE_NAME "help_tools"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsHelp_tools.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\libnlsHelp_tools_builtin.dll; DestDir: {app}\bin\{#BinPath}\;
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.nls; DestDir: {app}\modules\{#MODULE_NAME}\;
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.nls; DestDir: {app}\modules\{#MODULE_NAME}\etc\;
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.nls; DestDir: {app}\modules\{#MODULE_NAME}\etc\;
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\resources\about.txt; DestDir: {app}\modules\{#MODULE_NAME}\resources\;
Source: {#RootPath}modules\{#MODULE_NAME}\resources\highlight.pack.js; DestDir: {app}\modules\{#MODULE_NAME}\resources\;
Source: {#RootPath}modules\{#MODULE_NAME}\resources\mono-blue.css; DestDir: {app}\modules\{#MODULE_NAME}\resources\;
Source: {#RootPath}modules\{#MODULE_NAME}\resources\nelson_help_collection.qhc; DestDir: {app}\modules\{#MODULE_NAME}\resources\;
Source: {#RootPath}modules\{#MODULE_NAME}\resources\style.css; DestDir: {app}\modules\{#MODULE_NAME}\resources\;
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.nlf; DestDir: {app}\modules\{#MODULE_NAME}\functions\;
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.qch; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs
;==============================================================================
