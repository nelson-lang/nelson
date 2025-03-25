//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#include <Windows.h>
//=============================================================================
#pragma comment(lib, "libnlsblaslapack.lib")
//=============================================================================
#pragma comment(linker, "/export:mb05od_")
#pragma comment(linker, "/export:mb04md_")
#pragma comment(linker, "/export:mb04gd_")
#pragma comment(linker, "/export:mb03rd_")
#pragma comment(linker, "/export:mb03pd_")
#pragma comment(linker, "/export:mc01td_")
#pragma comment(linker, "/export:mb02md_")
#pragma comment(linker, "/export:ag08bd_")
#pragma comment(linker, "/export:ab08nd_")
#pragma comment(linker, "/export:ab07nd_")
#pragma comment(linker, "/export:ab04md_")
#pragma comment(linker, "/export:ab01od_")
#pragma comment(linker, "/export:sb01bd_")
#pragma comment(linker, "/export:tg01ad_")
#pragma comment(linker, "/export:tb01id_")
#pragma comment(linker, "/export:sg02ad_")
#pragma comment(linker, "/export:sb10jd_")
#pragma comment(linker, "/export:sb04qd_")
#pragma comment(linker, "/export:sb04md_")
#pragma comment(linker, "/export:sb03od_")
#pragma comment(linker, "/export:sb03md_")
#pragma comment(linker, "/export:mb03od_")
#pragma comment(linker, "/export:sb02od_")
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
#endif
