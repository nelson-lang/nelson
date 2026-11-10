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
#include "nlsOperators_exports.h"
#include "Types.hpp"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// transposed: A is the conjugate transpose of the matrix to factor (right division).
NLSOPERATORS_IMPEXP
ArrayOf
solveLinearEquationDouble(const ArrayOf& matA, const ArrayOf& matB, std::wstring& warningId,
    std::string& warningMessage, bool transposed = false);
//=============================================================================
NLSOPERATORS_IMPEXP
ArrayOf
solveLinearEquationDoubleComplex(const ArrayOf& matA, const ArrayOf& matB, std::wstring& warningId,
    std::string& warningMessage, bool transposed = false);
//=============================================================================
NLSOPERATORS_IMPEXP
ArrayOf
solveLinearEquationSingle(const ArrayOf& matA, const ArrayOf& matB, std::wstring& warningId,
    std::string& warningMessage, bool transposed = false);
//=============================================================================
NLSOPERATORS_IMPEXP
ArrayOf
solveLinearEquationSingleComplex(const ArrayOf& matA, const ArrayOf& matB, std::wstring& warningId,
    std::string& warningMessage, bool transposed = false);
//=============================================================================
// numerator / divisor for a dense square divisor of the same float class; false: not handled.
NLSOPERATORS_IMPEXP
bool
solveRightLinearEquation(const ArrayOf& numerator, const ArrayOf& divisor, ArrayOf& result,
    std::wstring& warningId, std::string& warningMessage);
//=============================================================================
} // namespace Nelson
//=============================================================================
