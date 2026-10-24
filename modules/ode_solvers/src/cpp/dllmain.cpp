//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Link the SUNDIALS libraries the native OdeCore uses (CVODES + IDAS + a dense
// direct linear solver with Newton). Kept here so the core library owns its own
// link dependencies, independent of the ode_solvers builtin gateway.
#if _MSC_VER && defined(NELSON_WITH_SUNDIALS)
#pragma comment(lib, "sundials_core.lib")
#pragma comment(lib, "sundials_cvodes.lib")
#pragma comment(lib, "sundials_idas.lib")
#pragma comment(lib, "sundials_nvecserial.lib")
#pragma comment(lib, "sundials_sunmatrixdense.lib")
#pragma comment(lib, "sundials_sunlinsoldense.lib")
#pragma comment(lib, "sundials_sunnonlinsolnewton.lib")
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
