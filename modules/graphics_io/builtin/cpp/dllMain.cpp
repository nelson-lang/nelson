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
#ifdef _DEBUG
#define DEBUG_SUFFIX "d"
#else
#define DEBUG_SUFFIX ""
#endif
//=============================================================================
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
#pragma comment(lib, "Qt6Svg" DEBUG_SUFFIX ".lib")
#pragma comment(lib, "Qt6Core" DEBUG_SUFFIX ".lib")
#pragma comment(lib, "Qt6Widgets" DEBUG_SUFFIX ".lib")
#pragma comment(lib, "Qt6Gui" DEBUG_SUFFIX ".lib")
#pragma comment(lib, "Qt6OpenGL" DEBUG_SUFFIX ".lib")
#pragma comment(lib, "Qt6OpenGLWidgets" DEBUG_SUFFIX ".lib")
#pragma comment(lib, "Qt6PrintSupport" DEBUG_SUFFIX ".lib")
#else
#pragma comment(lib, "Qt5Svg" DEBUG_SUFFIX ".lib")
#pragma comment(lib, "Qt5Core" DEBUG_SUFFIX ".lib")
#pragma comment(lib, "Qt5Widgets" DEBUG_SUFFIX ".lib")
#pragma comment(lib, "Qt5Gui" DEBUG_SUFFIX ".lib")
#pragma comment(lib, "Qt5OpenGL" DEBUG_SUFFIX ".lib")
#pragma comment(lib, "Qt5PrintSupport" DEBUG_SUFFIX ".lib")
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
