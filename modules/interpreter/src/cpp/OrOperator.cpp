//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Or.hpp"
#include "Evaluator.hpp"
#include "OverloadBinaryOperator.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Evaluator::orOperator(const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf retval;
    if ((overloadOnBasicTypes || needToOverloadOperator(A) || needToOverloadOperator(B))
        && isOverloadAllowed()) {
        retval = OverloadBinaryOperator(this, A, B, "or");
    } else {
        retval = Or(A, B);
    }
    return retval;
}
//=============================================================================
ArrayOf
Evaluator::orOperator(AbstractSyntaxTreePtr t)
{
    callstack.pushID((size_t)t->getContext());
    const ArrayOf A = expression(t->down);
    const ArrayOf B = expression(t->down->right);
    ArrayOf retval = orOperator(A, B);
    callstack.popID();
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
