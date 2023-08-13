//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Evaluator.hpp"
#include "Operators.hpp"
#include "OverloadBinaryOperator.hpp"
#include "DotPower.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Evaluator::powerOperator(AbstractSyntaxTreePtr t)
{
    callstack.pushID((size_t)t->getContext());
    ArrayOf retval = this->powerOperator(expression(t->down), expression(t->down->right));
    callstack.popID();
    return retval;
}
//=============================================================================
ArrayOf
Evaluator::powerOperator(const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf retval;
    bool needToOverload;
    ArrayOf res = DotPower(A, B, needToOverload);
    if (needToOverload) {
        OverloadRequired(POWER_OPERATOR_STR);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
