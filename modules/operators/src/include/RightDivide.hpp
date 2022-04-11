//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "nlsOperators_exports.h"
//=============================================================================
namespace Nelson {
/**
 * The right divide operation is related to the left divide operation
 * via: B/A = (A'\B')'.
 */
NLSOPERATORS_IMPEXP ArrayOf
RightDivide(ArrayOf A, ArrayOf B, bool& needToOverload);
//=============================================================================
} // namespace Nelson
//=============================================================================
