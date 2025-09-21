;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
; Qt 5.x & Qt 6.x
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\qt.conf; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\plugins\*; DestDir: {app}\bin\{#BinPath}\plugins; Flags: recursesubdirs;Components: {#COMPONENT_GUI};
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\qml\*; DestDir: {app}\bin\{#BinPath}\qml; Flags: recursesubdirs;Components: {#COMPONENT_GUI};
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\d3dcompiler_*.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
#ifdef QT5_USED
Source: {#RootPath}bin\{#BinPath}\libEGL.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\libGLESv2.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?3DExtras.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?3DInput.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?3DLogic.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?3DQuick.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?3DQuickExtras.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?3DQuickInput.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?3DQuickRender.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?3DRender.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?Multimedia.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?MultimediaWidgets.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?Sensors.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?WebChannel.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?WebSockets.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?WebView.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?WinExtras.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
#endif
Source: {#RootPath}bin\{#BinPath}\opengl32sw.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?*.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?Concurrent.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?Core.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?DBus.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?Gui.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?Help.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?Network.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?OpenGL.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?PrintSupport.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?Qml.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?Quick.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?QuickControls2.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?QuickParticles.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?QuickTemplates2.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?QuickWidgets.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?Help.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?Svg.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?WebEngine.dll; DestDir: {app}\bin\{#BinPath}\; Flags: skipifsourcedoesntexist;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?WebEngineCore.dll; DestDir: {app}\bin\{#BinPath}\; Flags: skipifsourcedoesntexist;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?WebEngineWidgets.dll; DestDir: {app}\bin\{#BinPath}\; Flags: skipifsourcedoesntexist;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?QmlWorkerScript.dll; DestDir: {app}\bin\{#BinPath}\; Flags: skipifsourcedoesntexist;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?QmlModels.dll; DestDir: {app}\bin\{#BinPath}\; Flags: skipifsourcedoesntexist;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt?Widgets.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
;==============================================================================
#ifdef QT6_USED
Source: {#RootPath}bin\{#BinPath}\Qt6QuickLayouts.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt6QuickControls2Basic.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt6QuickControls2Fusion.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt6QuickControls2FusionStyleImpl.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt6QmlMeta.dll; DestDir: {app}\bin\{#BinPath}\;Flags: skipifsourcedoesntexist;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt6QuickControls2WindowsStyleImpl.dll; DestDir: {app}\bin\{#BinPath}\;Flags: skipifsourcedoesntexist;Components: {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\Qt6QuickEffects.dll; DestDir: {app}\bin\{#BinPath}\;Flags: skipifsourcedoesntexist;Components: {#COMPONENT_GUI};
#endif
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\qhelpgenerator.exe; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GUI};
;==============================================================================
#ifdef QT5_USED
Source: {#RootPath}bin\{#BinPath}\Qt?CLucene.dll; DestDir: {app}\bin\{#BinPath}\; Flags: skipifsourcedoesntexist;Components: {#COMPONENT_GUI};
#endif
;==============================================================================
