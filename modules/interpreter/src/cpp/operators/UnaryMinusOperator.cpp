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
#include "UnaryMinus.hpp"
#include "OverloadHelpers.hpp"
#include "BinaryOperatorHelpers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
namespace Nelson {
ArrayOf
Evaluator::uminusOperator(const ArrayOf& A)
{
    bool wasFound = false;
    ArrayOf res = callOperatorOverload(this, A, UMINUS_OPERATOR_STR, wasFound);
    if (wasFound) {
        return res;
    }
    bool needToOverload = false;
    res = UnaryMinus(A, needToOverload);
    if (needToOverload) {
        OverloadRequired(UMINUS_OPERATOR_STR);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
