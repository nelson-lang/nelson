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
; Qt 5.x
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\qt.conf; DestDir: {app}\bin\{#BinPath}\
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\plugins\*; DestDir: {app}\bin\{#BinPath}\plugins; Flags: recursesubdirs
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\qml\*; DestDir: {app}\bin\{#BinPath}\qml; Flags: recursesubdirs
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\d3dcompiler_*.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\libEGL.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\libGLESv2.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\opengl32sw.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt5*.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt53DExtras.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt53DInput.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt53DLogic.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt53DQuick.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt53DQuickExtras.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt53DQuickInput.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt53DQuickRender.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt53DRender.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt5Concurrent.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt5Core.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt5DBus.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt5Gui.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt5Help.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt5Multimedia.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt5MultimediaWidgets.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt5Network.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt5OpenGL.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt5PrintSupport.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt5Qml.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt5Quick.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt5QuickControls2.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt5QuickParticles.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt5QuickTemplates2.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt5QuickWidgets.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt5Sensors.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt5Sql.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt5Svg.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt5WebChannel.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt5WebEngine.dll; DestDir: {app}\bin\{#BinPath}\; Flags: skipifsourcedoesntexist
Source: {#RootPath}bin\{#BinPath}\Qt5WebEngineCore.dll; DestDir: {app}\bin\{#BinPath}\; Flags: skipifsourcedoesntexist
Source: {#RootPath}bin\{#BinPath}\Qt5WebEngineWidgets.dll; DestDir: {app}\bin\{#BinPath}\; Flags: skipifsourcedoesntexist
Source: {#RootPath}bin\{#BinPath}\Qt5WebSockets.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt5WebView.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt5Widgets.dll; DestDir: {app}\bin\{#BinPath}\
Source: {#RootPath}bin\{#BinPath}\Qt5WinExtras.dll; DestDir: {app}\bin\{#BinPath}\
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\assistant.exe; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\Qt5CLucene.dll; DestDir: {app}\bin\{#BinPath}\; Flags: skipifsourcedoesntexist
Source: {#RootPath}bin\{#BinPath}\qcollectiongenerator.exe; DestDir: {app}\bin\{#BinPath}\;
Source: {#RootPath}bin\{#BinPath}\qhelpgenerator.exe; DestDir: {app}\bin\{#BinPath}\;
;==============================================================================
