//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <boost/version.hpp>
//=============================================================================
#include "boost_libraries_helpers.hpp"
//=============================================================================
#pragma comment(lib, CAT_3_STRINGS("boost_filesystem-", BOOST_TARGET, ".lib"))
#if BOOST_VERSION >= 108800
#pragma comment(lib, CAT_3_STRINGS("boost_process-", BOOST_TARGET, ".lib"))
#endif
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
