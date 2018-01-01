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
#define MODULE_NAME "functions_manager"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsFunctions_manager.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\libnlsFunctions_manager_builtin.dll; DestDir: {app}\bin\{#BinPath}\;
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.nls; DestDir: {app}\modules\{#MODULE_NAME}\;
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.nls; DestDir: {app}\modules\{#MODULE_NAME}\etc\;
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.nls; DestDir: {app}\modules\{#MODULE_NAME}\etc\;
;==============================================================================
;Source: {#RootPath}modules\functions_manager\functions\*.nlf; DestDir: {app}\modules\functions_manager\functions\;
;==============================================================================
Source: {#RootPath}modules\functions_manager\help\*.qch; DestDir: {app}\modules\functions_manager\help\; Flags: recursesubdirs
;==============================================================================
Source: {#RootPath}modules\functions_manager\tests\*.nls; DestDir: {app}\modules\functions_manager\tests\; Flags: recursesubdirs
;==============================================================================
