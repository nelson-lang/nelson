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
#include <string>
#include "nlsTypes_exports.h"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSTYPES_IMPEXP
bool
FindCommonConcatenateType(const ArrayOfVector& argIn, NelsonType& commonType, bool& isSparse,
    bool& isComplex, std::string& typeName);
//=============================================================================
/**
 * Prepare the missing operands of a concatenation: they become NaN when the
 * common class is double or single (also next to an empty operand, which
 * the concatenation would otherwise skip), and a class that has no missing
 * value (integer, logical, char, cell, struct, function handle) is an error.
 */
NLSTYPES_IMPEXP
void
prepareMissingConcatenation(ArrayOfVector& argIn, NelsonType commonType);
//=============================================================================
/**
 * Complex integers are not supported: when the common class is an integer,
 * a complex operand whose imaginary part does not convert to zero in that
 * class (rounded, NaN as zero, saturated) raises an error instead of losing
 * its imaginary part. Empty operands take no part.
 */
NLSTYPES_IMPEXP
void
checkComplexIntegerConcatenation(const ArrayOfVector& argIn, NelsonType commonType);
//=============================================================================
} // namespace Nelson
//=============================================================================
