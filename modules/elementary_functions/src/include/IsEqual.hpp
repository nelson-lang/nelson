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
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSELEMENTARY_FUNCTIONS_IMPEXP bool
IsEqual(Evaluator* eval, const ArrayOfVector& args, NelsonType commonType, bool isSparse,
    bool isComplex, bool& needToOverload);
//=============================================================================
// Class the arguments are compared in: the common type, except that values of
// mixed classes sharing an integer (up to 32 bits) or single common type are
// compared exactly, as double, instead of being rounded to the narrower class.
NLSELEMENTARY_FUNCTIONS_IMPEXP NelsonType
IsEqualComparisonType(const ArrayOfVector& args, NelsonType commonType);
//=============================================================================
// Converts one argument to the comparison class. Returns false when a value
// of its class can never be equal to a value of the comparison class.
NLSELEMENTARY_FUNCTIONS_IMPEXP bool
IsEqualConvertArgument(
    const ArrayOf& value, NelsonType comparisonType, bool isSparse, ArrayOf& converted);
//=============================================================================
// True when the value is not already of the comparison class (and sparse
// state): the common case needs no conversion and no copy.
inline bool
IsEqualNeedsConversion(const ArrayOf& value, NelsonType comparisonType, bool isSparse)
{
    return value.dataClass() != comparisonType || (isSparse && !value.isSparse());
}
//=============================================================================
} // namespace Nelson
//=============================================================================
