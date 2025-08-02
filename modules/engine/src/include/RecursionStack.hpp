//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <cstdlib>
#include "nlsEngine_exports.h"
//=============================================================================
namespace Nelson {
#define SIZE_MAX_RECURSION_CALL (60 * 1024 * 1024)
// SIZE_MAX_RECURSION_CALL OS dependant current value works on linux, mac, windows
// On Windows, you need to set /STACK:reserce
#define MAX_RECURSION_FUNCTION_CALL 1936
// ideally, it should be 5000
#define DEFAULT_RECURSION_FUNCTION_CALL 500
NLSENGINE_IMPEXP size_t
setRecursionStacksize(size_t sizemb);
NLSENGINE_IMPEXP size_t
getRecursionStacksize();
} // namespace Nelson
//=============================================================================
