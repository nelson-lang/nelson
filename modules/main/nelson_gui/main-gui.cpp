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
#include <shellapi.h>
#include <cstdio>
#include <string>
#include "WebDesktopBridge.hpp"
//=============================================================================
static bool
isBatchRun();
//=============================================================================
int WINAPI
wWinMain(HINSTANCE, HINSTANCE, PWSTR, int)
{
    const int exitCode = runNelson(__argc, __argv, NELSON_ENGINE_MODE::GUI);
    if (Nelson::WebDesktopBridge::getInstance()->wasWebViewUnavailableAtLaunch()) {
        // A batch run has nobody to close a dialog: fail without waiting.
        if (isBatchRun()) {
            std::fprintf(stderr, "Nelson: batch run stopped, no WebView is available.\n");
            std::fflush(stderr);
            return exitCode != 0 ? exitCode : 1;
        }
        MessageBoxW(nullptr, L"No WebView is available.", L"Nelson", MB_ICONERROR | MB_OK);
    }
    return exitCode;
}
//=============================================================================
// True when a command or a file to execute was given on the command line.
static bool
isBatchRun()
{
    int argCount = 0;
    LPWSTR* arguments = CommandLineToArgvW(GetCommandLineW(), &argCount);
    if (arguments == nullptr) {
        return false;
    }
    bool batch = false;
    for (int k = 1; k < argCount && !batch; ++k) {
        const std::wstring argument(arguments[k]);
        for (const wchar_t* name : { L"--execute", L"--file", L"--file-ipc" }) {
            const std::wstring option(name);
            batch = batch || argument == option || argument.rfind(option + L"=", 0) == 0;
        }
        batch = batch || argument == L"-e" || argument == L"-f" || argument == L"-F";
    }
    LocalFree(arguments);
    return batch;
}
//=============================================================================
#else
int
main(int argc, char* argv[])
{
    return runNelson(argc, argv, NELSON_ENGINE_MODE::GUI);
}
//=============================================================================
#endif
//=============================================================================
