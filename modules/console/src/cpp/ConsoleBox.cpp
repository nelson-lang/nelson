//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ConsoleBox.hpp"
#ifdef _MSC_VER
#include <fcntl.h>
#include <io.h>
#include <cstdio>
#include <mutex>
#include <string>
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include "Nelson_VERSION.h"
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
// Show/hide of the attached OS console window. This used the Qt desktop library
// (loaded dynamically); it is now served directly with the Win32 console API, so
// the console module carries no toolkit dependency. The GUI process is a windows
// subsystem app with no console, so the window is allocated on first use.
//=============================================================================
#ifdef _MSC_VER
//=============================================================================
static std::once_flag consoleInitFlag;
//=============================================================================
// Allocate a hidden console the first time the feature is used and route the C
// standard streams to it, so native output shows once the window is revealed.
static void
allocateConsoleWindow()
{
    if (!AllocConsole()) {
        return;
    }
    FILE* fpIn = nullptr;
    FILE* fpOut = nullptr;
    FILE* fpErr = nullptr;
    if (freopen_s(&fpIn, "CONIN$", "r", stdin) != 0) {
        fpIn = nullptr;
    }
    if (freopen_s(&fpOut, "CONOUT$", "w", stdout) != 0) {
        fpOut = nullptr;
    }
    if (freopen_s(&fpErr, "CONOUT$", "w", stderr) != 0) {
        fpErr = nullptr;
    }
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
    HWND consoleWindow = GetConsoleWindow();
    if (consoleWindow) {
        ShowWindow(consoleWindow, SW_HIDE);
        // Disable the Close (X) button so the console cannot be closed with it.
        HMENU systemMenu = GetSystemMenu(consoleWindow, FALSE);
        if (systemMenu) {
            EnableMenuItem(systemMenu, SC_CLOSE, MF_BYCOMMAND | MF_GRAYED);
            DrawMenuBar(consoleWindow);
        }
        std::string title = "Nelson - " + std::string(NELSON_VERSION_STRING);
        SetConsoleTitleA(title.c_str());
        SetWindowTextA(consoleWindow, title.c_str());
    }
}
//=============================================================================
// Return the console window, allocating one on first access when none exists.
static HWND
ensureConsoleWindow()
{
    if (GetConsoleWindow() == nullptr) {
        std::call_once(consoleInitFlag, allocateConsoleWindow);
    }
    return GetConsoleWindow();
}
#endif
//=============================================================================
bool
ConsoleBoxShow(bool show)
{
#ifdef _MSC_VER
    HWND consoleWindow = show ? ensureConsoleWindow() : GetConsoleWindow();
    if (consoleWindow) {
        ShowWindow(consoleWindow, show ? SW_SHOW : SW_HIDE);
        return ConsoleBoxIsVisible();
    }
#endif
    return false;
}
//=============================================================================
bool
ConsoleBoxToggle()
{
    return ConsoleBoxShow(!ConsoleBoxIsVisible());
}
//=============================================================================
bool
ConsoleBoxIsVisible()
{
#ifdef _MSC_VER
    HWND consoleWindow = GetConsoleWindow();
    if (consoleWindow) {
        return IsWindowVisible(consoleWindow) != 0;
    }
#endif
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
