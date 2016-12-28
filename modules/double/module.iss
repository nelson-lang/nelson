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
Source: {#RootPath}bin\{#BinPath}\libnlsDouble.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\libnlsDouble_builtin.dll; DestDir: {app}\bin\{#BinPath}\;
;==============================================================================
Source: {#RootPath}modules\double\loader.nls; DestDir: {app}\modules\double\;
Source: {#RootPath}modules\double\etc\startup.nls; DestDir: {app}\modules\double\etc\;
Source: {#RootPath}modules\double\etc\finish.nls; DestDir: {app}\modules\double\etc\;
;==============================================================================
Source: {#RootPath}modules\double\functions\*.nlf; DestDir: {app}\modules\double\functions\;
;==============================================================================
Source: {#RootPath}modules\double\help\*.qch; DestDir: {app}\modules\double\help\; Flags: recursesubdirs
;==============================================================================
Source: {#RootPath}modules\double\tests\*.nls; DestDir: {app}\modules\double\tests\; Flags: recursesubdirs
Source: {#RootPath}modules\double\tests\*.ref; DestDir: {app}\modules\double\tests\; Flags: recursesubdirs
;==============================================================================
