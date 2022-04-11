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
#ifdef _MSC_VER
int WINAPI WinMain(HINSTANCE hInstance,
                   HINSTANCE hPrevInstance,
                   LPSTR lpCmdLine,
                   int nCmdShow)
#else
int main(int argc, char *argv[])
#endif
{
#ifdef _MSC_VER
    int argCount = 0;
    LPWSTR *szArgList = CommandLineToArgvW(GetCommandLineW(), &argCount);
#ifndef _DEBUG
    /* catch system errors msgbox (release mode only) */
    /* http://msdn.microsoft.com/en-us/library/ms680621(VS.85).aspx */
    UINT LastErrorMode = SetErrorMode(SEM_FAILCRITICALERRORS | SEM_NOALIGNMENTFAULTEXCEPT | SEM_NOGPFAULTERRORBOX);
#endif
#endif
#ifdef _MSC_VER
    int ierr = StartNelson(argCount, szArgList, NELSON_ENGINE_MODE::GUI);
    LocalFree(szArgList);
#else
    int ierr = StartNelson(argc, argv, NELSON_ENGINE_MODE::GUI);
#endif
    return ierr;
}
//=============================================================================
