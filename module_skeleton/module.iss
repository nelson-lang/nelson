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
#define MODULE_NAME "module_skeleton"
;==============================================================================
Source: {#RootPath}\{#MODULE_NAME}\loader.nls; DestDir: {app}\{#MODULE_NAME}\;
Source: {#RootPath}\{#MODULE_NAME}\builder.nls; DestDir: {app}\{#MODULE_NAME}\;
;==============================================================================
Source: {#RootPath}\{#MODULE_NAME}\etc\startup.nls; DestDir: {app}\{#MODULE_NAME}\etc\;
Source: {#RootPath}\{#MODULE_NAME}\etc\finish.nls; DestDir: {app}\{#MODULE_NAME}\etc\;
;==============================================================================
Source: {#RootPath}\{#MODULE_NAME}\functions\*.nlf; DestDir: {app}\{#MODULE_NAME}\functions\;
;==============================================================================
Source: {#RootPath}\{#MODULE_NAME}\help\en_US\images\*.png; DestDir: {app}\{#MODULE_NAME}\help\en_US\images\;
Source: {#RootPath}\{#MODULE_NAME}\help\en_US\xml\*.xml; DestDir: {app}\{#MODULE_NAME}\help\en_US\xml\;
;==============================================================================
Source: {#RootPath}\{#MODULE_NAME}\src\*.*; DestDir: {app}\{#MODULE_NAME}\src\; Flags: recursesubdirs
Source: {#RootPath}\{#MODULE_NAME}\builtin\*.*; DestDir: {app}\{#MODULE_NAME}\builtin\; Flags: recursesubdirs
;==============================================================================
Source: {#RootPath}\{#MODULE_NAME}\tests\*.nls; DestDir: {app}\{#MODULE_NAME}\tests\; Flags: recursesubdirs
;==============================================================================
