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
#include "nlsOperators_exports.h"
#include "Types.hpp"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSOPERATORS_IMPEXP
ArrayOf
solveSVDDecompositionDouble(const ArrayOf& matA, const ArrayOf& matB);
//=============================================================================
NLSOPERATORS_IMPEXP
ArrayOf
solveSVDDecompositionDoubleComplex(const ArrayOf& matA, const ArrayOf& matB);
//=============================================================================
NLSOPERATORS_IMPEXP
ArrayOf
solveSVDDecompositionSingle(const ArrayOf& matA, const ArrayOf& matB);
//=============================================================================
NLSOPERATORS_IMPEXP
ArrayOf
solveSVDDecompositionSingleComplex(const ArrayOf& matA, const ArrayOf& matB);
//=============================================================================
}
//=============================================================================
