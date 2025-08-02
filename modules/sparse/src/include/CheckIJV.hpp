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
#include "nlsSparse_exports.h"
#include <cstdlib>
//=============================================================================
namespace Nelson {
NLSSPARSE_IMPEXP bool
CheckIJV(
    size_t ilen, size_t jlen, size_t vlen, int& istride, int& jstride, int& vstride, size_t& olen);
}
//=============================================================================
