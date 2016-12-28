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
; Qt platforms
Source: {#RootPath}bin\{#BinPath}\platforms\qminimal.dll; DestDir: {app}\bin\{#BinPath}\platforms\;
Source: {#RootPath}bin\{#BinPath}\platforms\qoffscreen.dll; DestDir: {app}\bin\{#BinPath}\platforms\;
Source: {#RootPath}bin\{#BinPath}\platforms\qwindows.dll; DestDir: {app}\bin\{#BinPath}\platforms\;
;==============================================================================
; Qt 5.6 Gui
Source: {#RootPath}bin\{#BinPath}\icudt56.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\icuuc56.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\Qt5Widgets.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\Qt5Core.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\Qt5Gui.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\Qt5PrintSupport.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\libEGL.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\libGLESv2.dll; DestDir: {app}\bin\{#BinPath}\;
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsGui.dll; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\libnlsGui_builtin.dll; DestDir: {app}\bin\{#BinPath}\;
;==============================================================================
Source: {#RootPath}modules\gui\loader.nls; DestDir: {app}\modules\gui\;
Source: {#RootPath}modules\gui\etc\startup.nls; DestDir: {app}\modules\gui\etc\;
Source: {#RootPath}modules\gui\etc\finish.nls; DestDir: {app}\modules\gui\etc\;
;==============================================================================
;Source: {#RootPath}modules\gui\functions\*.nlf; DestDir: {app}\modules\gui\functions\;
;==============================================================================
Source: {#RootPath}modules\gui\help\*.qch; DestDir: {app}\modules\gui\help\; Flags: recursesubdirs
;==============================================================================

