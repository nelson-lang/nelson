//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#if _MSC_VER
#include <fcntl.h>
#include <io.h>
#include <shlwapi.h>
#include <cstdio>
#include <cstdlib>
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <winver.h>
#include <string>
#include <vector>
#define dup _dup
#define dup2 _dup2
#define close _close
#define fileno _fileno
#endif
#include "Console.hpp"
#include "Nelson_VERSION.h"
//===================================================================================
namespace Nelson {
//===================================================================================
bool
CreateConsole()
{
    bool res = false;
#if _MSC_VER
    if (!AllocConsole()) {
        return false;
    }
    HWND hNelSonConsole = GetConsoleWindow();

    FILE* fpIn = nullptr;
    FILE* fpOut = nullptr;
    FILE* fpErr = nullptr;

    // Use secure variants and check results
    if (freopen_s(&fpIn, "CONIN$", "r", stdin) != 0 || fpIn == nullptr) {
        // attempt fallback: leave stdin untouched
    }
    if (freopen_s(&fpOut, "CONOUT$", "w", stdout) != 0 || fpOut == nullptr) {
        // attempt fallback
    }
    if (freopen_s(&fpErr, "CONOUT$", "w", stderr) != 0 || fpErr == nullptr) {
        // attempt fallback
    }

    // Ensure text mode and disable buffering for interactive output
    if (fpOut) {
        _setmode(_fileno(fpOut), _O_TEXT);
        setvbuf(fpOut, nullptr, _IONBF, 0);
    }
    if (fpErr) {
        _setmode(_fileno(fpErr), _O_TEXT);
        setvbuf(fpErr, nullptr, _IONBF, 0);
    }
    if (fpIn) {
        _setmode(_fileno(fpIn), _O_TEXT);
    }

    if (hNelSonConsole) {
        ShowWindow(hNelSonConsole, SW_HIDE);

        // Disable the Close (X) button on the console window to prevent closing via the cross button
        HMENU hMenu = GetSystemMenu(hNelSonConsole, FALSE);
        if (hMenu) {
            // Gray out the Close menu item
            EnableMenuItem(hMenu, SC_CLOSE, MF_BYCOMMAND | MF_GRAYED);
            // Update the window menu bar
            DrawMenuBar(hNelSonConsole);
        }

        // Set console title to "Nelson - <version>"
        std::string title = "Nelson - " + std::string(NELSON_VERSION_STRING);
        SetConsoleTitleA(title.c_str());
        SetWindowTextA(hNelSonConsole, title.c_str());
    }

    // success if at least stdout or stderr reopened
    res = (fpOut != nullptr) || (fpErr != nullptr);
#endif
    return res;
}
//===================================================================================
bool
DestroyConsole()
{
#if _MSC_VER
    return FreeConsole() != 0;
#endif
    return false;
}
//===================================================================================
} // namespace Nelson
//===================================================================================
bool
ShowConsoleWindow(bool show)
{
#if _MSC_VER
    HWND h = GetConsoleWindow();
    if (h) {
        if (show) {
            ShowWindow(h, SW_SHOW);
        } else {
            ShowWindow(h, SW_HIDE);
        }
        return true;
    }
#endif
    return false;
}
//===================================================================================
bool
ConsoleIsVisible()
{
#if _MSC_VER
    HWND h = GetConsoleWindow();
    if (h) {
        return IsWindowVisible(h) != 0;
    }
#endif
    return false;
}
//===================================================================================
bool
ToggleConsole()
{
    return ShowConsoleWindow(!ConsoleIsVisible());
}
//===================================================================================
