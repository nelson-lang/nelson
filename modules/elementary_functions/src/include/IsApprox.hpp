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
#include "nlsElementary_functions_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
enum class IsApproxLevel
{
    VeryTight,
    Tight,
    Loose,
    VeryLoose
};
//=============================================================================
// default tolerance of a level, single precision when an input is single
NLSELEMENTARY_FUNCTIONS_IMPEXP double
IsApproxLevelTolerance(IsApproxLevel level, bool singlePrecision);
//=============================================================================
// int64 and uint64 values must stay within flintmax to be compared exactly
NLSELEMENTARY_FUNCTIONS_IMPEXP void
IsApproxCheckIntegerRange(const ArrayOf& A, const ArrayOf& B);
//=============================================================================
// element-wise |A-B| <= max(absTol, relTol*max(|A|,|B|)) with implicit expansion
NLSELEMENTARY_FUNCTIONS_IMPEXP ArrayOf
IsApprox(const ArrayOf& A, const ArrayOf& B, const ArrayOf& absoluteTolerance,
    const ArrayOf& relativeTolerance);
//=============================================================================
} // namespace Nelson
//=============================================================================
