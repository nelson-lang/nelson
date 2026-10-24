//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#if _MSC_VER && defined(NELSON_WITH_SUNDIALS)
#pragma comment(lib, "sundials_core.lib")
#pragma comment(lib, "sundials_cvodes.lib")
#pragma comment(lib, "sundials_idas.lib")
#pragma comment(lib, "sundials_nvecserial.lib")
#pragma comment(lib, "sundials_sunmatrixdense.lib")
#pragma comment(lib, "sundials_sunlinsoldense.lib")
#pragma comment(lib, "sundials_sunnonlinsolnewton.lib")
#pragma comment(lib, "sundials_sunmatrixsparse.lib")
#pragma comment(lib, "sundials_sunlinsolspgmr.lib")
#pragma comment(lib, "sundials_sunlinsolspfgmr.lib")
#pragma comment(lib, "sundials_sunlinsolspbcgs.lib")
#pragma comment(lib, "sundials_sunlinsolsptfqmr.lib")
#pragma comment(lib, "sundials_sunlinsolpcg.lib")
#pragma comment(lib, "sundials_kinsol.lib")
#endif
//=============================================================================
#ifdef _WIN32
//=============================================================================
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
//=============================================================================
BOOL APIENTRY
DllMain(HINSTANCE hInstance, DWORD reason, PVOID pvReserved)
{
    UNREFERENCED_PARAMETER(hInstance);
    UNREFERENCED_PARAMETER(pvReserved);
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
    return TRUE;
}
#endif
//=============================================================================
