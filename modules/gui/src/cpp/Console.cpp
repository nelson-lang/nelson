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
#define _CRT_SECURE_NO_WARNINGS
#include <fcntl.h>
#include <io.h>
#include <shlwapi.h>
#include <cstdio>
#include <cstdlib>
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#define dup _dup
#define dup2 _dup2
#define close _close
#define fileno _fileno
#endif
#include "Console.hpp"
//===================================================================================
namespace Nelson {
//===================================================================================
bool
CreateConsole()
{
    bool res = false;
#if _MSC_VER
    if (AllocConsole()) {
        HWND hNelSonConsole = GetConsoleWindow();
        FILE* fpCONIN = freopen("CONIN$", "r", stdin);
        FILE* fpCONOUT = freopen("CONOUT$", "w", stdout);
        FILE* fpCONERR = freopen("CONOUT$", "w", stderr);
        if (hNelSonConsole) {
            ShowWindow(hNelSonConsole, SW_HIDE);
        }
        res = true;
    }
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
