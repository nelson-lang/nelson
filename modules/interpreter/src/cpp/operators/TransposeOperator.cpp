//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Evaluator.hpp"
#include "Operators.hpp"
#include "OverloadHelpers.hpp"
#include "BinaryOperatorHelpers.hpp"
#include "OverloadRequired.hpp"
#include "Transpose.hpp"
//=============================================================================
namespace Nelson {
ArrayOf
Evaluator::transposeOperator(const ArrayOf& A)
{
    bool wasFound = false;
    ArrayOf res = callOperatorOverload(this, A, TRANSPOSE_OPERATOR_STR, wasFound);
    if (wasFound) {
        return res;
    }
    bool needToOverload = false;
    res = Transpose(A, needToOverload);
    if (needToOverload) {
        OverloadRequired(TRANSPOSE_OPERATOR_STR);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
