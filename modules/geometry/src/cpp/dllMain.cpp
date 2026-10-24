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
//=============================================================================
// Only the reentrant C library is linked: qhullcpp is a release-only static
// library and would clash with the debug C runtime.
#pragma comment(lib, "qhull_r.lib")
//=============================================================================
