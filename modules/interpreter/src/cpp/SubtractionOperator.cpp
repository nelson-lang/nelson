//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Subtraction.hpp"
#include "Evaluator.hpp"
#include "OverloadBinaryOperator.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Evaluator::subtractionOperator(AbstractSyntaxTreePtr t)
{
    callstack.pushID((size_t)t->getContext());
    ArrayOf retval = this->subtractionOperator(expression(t->down), expression(t->down->right));
    callstack.popID();
    return retval;
}
//=============================================================================
ArrayOf
Evaluator::subtractionOperator(const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    bool bSuccess = false;
    if (overloadOnBasicTypes) {
        res = OverloadBinaryOperator(this, A, B, "minus", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload;
        res = Subtraction(A, B, needToOverload);
        if (needToOverload) {
            return OverloadBinaryOperator(this, A, B, "minus");
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
