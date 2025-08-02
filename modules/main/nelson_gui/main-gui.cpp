//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#endif
#include <stdlib.h>
#include "NelsonRunner.h"
//=============================================================================
#ifdef _MSC_VER
int WINAPI
WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
#else
int
main(int argc, char* argv[])
#endif
{
#ifdef _MSC_VER
    return runNelson(__argc, __argv, NELSON_ENGINE_MODE::GUI);
#else
    return runNelson(argc, argv, NELSON_ENGINE_MODE::GUI);
#endif
}
//=============================================================================
