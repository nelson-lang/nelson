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
#include "nlsLinear_algebra_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Page-wise transpose (pagetranspose): transposes the first two dimensions of
// each page of an N-D array. Pure element rearrangement (no conjugation).
NLSLINEAR_ALGEBRA_IMPEXP ArrayOf
PageTranspose(const ArrayOf& X, bool& needToOverload);
//=============================================================================
} // namespace Nelson
//=============================================================================
