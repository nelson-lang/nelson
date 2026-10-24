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
#include "DotPower.hpp"
#include "BinaryOperatorHelpers.hpp"
//=============================================================================
namespace Nelson {
ArrayOf
Evaluator::powerOperator(const ArrayOfVector& args)
{
    const std::string functionName = POWER_OPERATOR_STR;
    BinaryOperatorCommonType commonType;

    ArrayOf res;
    if (tryCallBinaryOperatorOverload(this, args, functionName, commonType, res)) {
        return res;
    }

    bool needToOverload = false;
    ArrayOf A(args[0]);
    ArrayOf B(args[1]);
    prepareBinaryPowerOperands(commonType, A, B);
    applyBinaryOperatorSparseMode(BinaryOperatorSparseMode::BothOperands, commonType, A, B);
    res = DotPower(A, B, needToOverload);

    if (needToOverload) {
        return callBinaryOperatorOverloadOrRequire(this, args, functionName, commonType);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
