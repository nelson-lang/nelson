//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#include <Windows.h>
#endif
#include "StartNelson.h"
//=============================================================================
int main(int argc, char *argv[])
{
#ifdef _MSC_VER
    int argCount = 0;
    LPWSTR *szArgList = CommandLineToArgvW(GetCommandLineW(), &argCount);
    int ierr = StartNelson(argCount, szArgList, NELSON_ENGINE_MODE::ADVANCED_TERMINAL);
    LocalFree(szArgList);
    return ierr;
#else
    return StartNelson(argc, argv, NELSON_ENGINE_MODE::ADVANCED_TERMINAL);
#endif
}
//=============================================================================
