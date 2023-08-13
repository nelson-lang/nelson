//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
#include "OverloadBinaryOperator.hpp"
#include "RightDivide.hpp"
#include "Operators.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Evaluator::rightDivideOperator(AbstractSyntaxTreePtr t)
{
    callstack.pushID((size_t)t->getContext());
    ArrayOf retval = this->rightDivideOperator(expression(t->down), expression(t->down->right));
    callstack.popID();
    return retval;
}
//=============================================================================
ArrayOf
Evaluator::rightDivideOperator(const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res = {};
    bool bSuccess = false;
    if ((overloadOnBasicTypes || needToOverloadOperator(A) || needToOverloadOperator(B))
        && !isOverloadAllowed()) {
        res = OverloadBinaryOperator(this, A, B, MRDIVIDE_OPERATOR_STR, bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload = false;
        res = RightDivide(A, B, needToOverload);
        if (needToOverload) {
            res = OverloadBinaryOperator(this, A, B, MRDIVIDE_OPERATOR_STR);
        }
    }
    return res;
}
//=============================================================================
}
//=============================================================================
