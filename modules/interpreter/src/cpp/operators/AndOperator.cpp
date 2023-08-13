//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "And.hpp"
#include "Evaluator.hpp"
#include "OverloadBinaryOperator.hpp"
#include "Operators.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Evaluator::andOperator(const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf retval;
    if ((overloadOnBasicTypes || needToOverloadOperator(A) || needToOverloadOperator(B))
        && isOverloadAllowed()) {
        retval = OverloadBinaryOperator(this, A, B, AND_OPERATOR_STR);
    } else {
        retval = And(A, B);
    }
    return retval;
}
//=============================================================================
ArrayOf
Evaluator::andOperator(AbstractSyntaxTreePtr t)
{
    callstack.pushID((size_t)t->getContext());
    ArrayOf A = expression(t->down);
    ArrayOf B = expression(t->down->right);
    ArrayOf retval = andOperator(A, B);
    callstack.popID();
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
