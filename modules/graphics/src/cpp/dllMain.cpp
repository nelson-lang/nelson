//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <QtCore/QtGlobal>
//=============================================================================
#pragma comment(lib, "Opengl32.lib")
#pragma comment(lib, "Glu32.lib")
//=============================================================================
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
#pragma comment(lib, "Qt6Svg.lib")
#pragma comment(lib, "Qt6Core.lib")
#pragma comment(lib, "Qt6Widgets.lib")
#pragma comment(lib, "Qt6Gui.lib")
#pragma comment(lib, "Qt6OpenGL.lib")
#pragma comment(lib, "Qt6OpenGLWidgets.lib")
#pragma comment(lib, "Qt6PrintSupport.lib")
#else
#pragma comment(lib, "Qt5Svg.lib")
#pragma comment(lib, "Qt5Core.lib")
#pragma comment(lib, "Qt5Widgets.lib")
#pragma comment(lib, "Qt5Gui.lib")
#pragma comment(lib, "Qt5OpenGL.lib")
#pragma comment(lib, "Qt5PrintSupport.lib")
#endif
//=============================================================================
int WINAPI
DllMain(HINSTANCE hInstance, DWORD reason, PVOID pvReserved)
{
    switch (reason) {
    case DLL_PROCESS_ATTACH:
        break;
    case DLL_PROCESS_DETACH:
        break;
    case DLL_THREAD_ATTACH:
        break;
    case DLL_THREAD_DETACH:
        break;
    }
    return 1;
}
//=============================================================================
