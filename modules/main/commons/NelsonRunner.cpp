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
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <shellapi.h>
#include "StartNelsonDynamicFunction.hpp"
#else
#include "StartNelson.h"
#endif
#include "NelsonRunner.h"
//=============================================================================
int
runNelson(int argc, char* argv[], NELSON_ENGINE_MODE mode)
{
#ifdef _MSC_VER
    int argCount = 0;
    LPWSTR* szArgList = CommandLineToArgvW(GetCommandLineW(), &argCount);
#ifndef _DEBUG
    /* Catch system error messages (release mode only) */
    UINT LastErrorMode
        = SetErrorMode(SEM_FAILCRITICALERRORS | SEM_NOALIGNMENTFAULTEXCEPT | SEM_NOGPFAULTERRORBOX);
#endif
    int ierr = Nelson::StartNelsonDynamicFunction(argCount, (wchar_t**)szArgList, mode);
    LocalFree(szArgList);
#else
    int ierr = StartNelsonA(argc, argv, mode);
#endif
    return ierr;
}
//=============================================================================
