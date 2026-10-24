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
#include "ArrayOf.hpp"
#include "nlsSpecial_functions_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Regularized lower/upper incomplete gamma function.
//   isLower == true  -> P(x, a) = gammainc(x, a)
//   isLower == false -> Q(x, a) = gammainc(x, a, 'upper')
NLSSPECIAL_FUNCTIONS_IMPEXP ArrayOf
GammaIncomplete(const ArrayOf& X, const ArrayOf& A, bool isLower, bool& needToOverload);
//=============================================================================
} // namespace Nelson
//=============================================================================
