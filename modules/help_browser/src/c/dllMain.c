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
#pragma comment(lib, "shlwapi.lib") // AllocConsole
#ifdef _DEBUG
#pragma comment(lib, "boost_system-vc141-mt-gd-1_65_1.lib")
#pragma comment(lib, "boost_filesystem-vc141-mt-gd-1_65_1.lib")
#pragma comment(lib, "boost_chrono-vc141-mt-gd-1_65_1.lib")
#pragma comment(lib, "boost_thread-vc141-mt-gd-1_65_1.lib")
#pragma comment(lib, "boost_date_time-vc141-mt-gd-1_65_1.lib")
#pragma comment(lib, "boost_regex-vc141-mt-gd-1_65_1.lib")
#else
#pragma comment(lib, "boost_system-vc141-mt-1_65_1.lib")
#pragma comment(lib, "boost_filesystem-vc141-mt-1_65_1.lib")
#pragma comment(lib, "boost_chrono-vc141-mt-1_65_1.lib")
#pragma comment(lib, "boost_thread-vc141-mt-1_65_1.lib")
#pragma comment(lib, "boost_date_time-vc141-mt-1_65_1.lib")
#pragma comment(lib, "boost_regex-vc141-mt-1_65_1.lib")
#endif
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
