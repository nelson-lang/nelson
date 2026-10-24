//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonRunner.h"
//=============================================================================
#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include "WebDesktopBridge.hpp"
//=============================================================================
int WINAPI
wWinMain(HINSTANCE, HINSTANCE, PWSTR, int)
{
    const int exitCode = runNelson(__argc, __argv, NELSON_ENGINE_MODE::WEB_GUI);
    if (Nelson::WebDesktopBridge::getInstance()->wasWebViewUnavailableAtLaunch()) {
        MessageBoxW(nullptr, L"No WebView is available.", L"Nelson WebView", MB_ICONERROR | MB_OK);
    }
    return exitCode;
}
//=============================================================================
#else
int
main(int argc, char* argv[])
{
    return runNelson(argc, argv, NELSON_ENGINE_MODE::WEB_GUI);
}
//=============================================================================
#endif
//=============================================================================
