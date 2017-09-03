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
#include <Windows.h>
//=============================================================================
#ifdef _DEBUG
#pragma comment(lib, "boost_system-vc141-mt-gd-1_64.lib")
#pragma comment(lib, "boost_filesystem-vc141-mt-gd-1_64.lib")
#else
#pragma comment(lib, "boost_system-vc141-mt-1_64.lib")
#pragma comment(lib, "boost_filesystem-vc141-mt-1_64.lib")
#endif
//=============================================================================
#pragma comment(lib, "libopenblas.lib")
//=============================================================================
int WINAPI DllMain(HINSTANCE hInstance, DWORD reason, PVOID pvReserved)
{
    switch (reason)
    {
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
