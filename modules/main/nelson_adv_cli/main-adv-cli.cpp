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
#endif
#include "StartNelson.h"
//=============================================================================
int main(int argc, char *argv[])
{
#ifdef _MSC_VER
#ifndef _DEBUG
    /* catch system errors msgbox (release mode only) */
    /* http://msdn.microsoft.com/en-us/library/ms680621(VS.85).aspx */
    UINT LastErrorMode = SetErrorMode(SEM_FAILCRITICALERRORS | SEM_NOALIGNMENTFAULTEXCEPT | SEM_NOGPFAULTERRORBOX);
#endif
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
