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
#include "ElementWiseMultiplication.hpp"
#include "BinaryOperatorHelpers.hpp"
#include "Operators.hpp"
//=============================================================================
namespace Nelson {
ArrayOf
Evaluator::timesOperator(const ArrayOfVector& args)
{
    const std::string functionName = TIMES_OPERATOR_STR;
    BinaryOperatorCommonType commonType;

    ArrayOf res;
    if (tryCallBinaryOperatorOverload(this, args, functionName, commonType, res)) {
        return res;
    }

    bool needToOverload = false;
    ArrayOf A(args[0]);
    ArrayOf B(args[1]);
    // A .* B: scale a complex operand by a real one without promoting it.
    if (dotTimesComplexRealScaling(A, B, res)) {
        return res;
    }
    prepareBinaryNumericOperands(commonType, A, B);
    applyBinaryOperatorSparseMode(BinaryOperatorSparseMode::BothOperands, commonType, A, B);
    res = elementWiseMultiplication(A, B, needToOverload);

    if (needToOverload) {
        return callBinaryOperatorOverloadOrRequire(this, args, functionName, commonType);
    }

    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
