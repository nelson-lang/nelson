//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
