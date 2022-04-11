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
#include "OverloadBinaryOperator.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Evaluator::shortCutAndOperator(const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf retval;
    if ((overloadOnBasicTypes || needToOverloadOperator(A) || needToOverloadOperator(B))
        && !isOverloadAllowed()) {
        retval = OverloadBinaryOperator(this, A, B, "shortcutand");
    } else {
        if (A.isScalar()) {
            bool a = A.getContentAsLogicalScalar() != 0U;
            if (!a) {
                retval = A;
            } else {
                if (B.isScalar()) {
                    bool b = B.getContentAsLogicalScalar() != 0U;
                    return ArrayOf::logicalConstructor(b);
                }
                Error(_W("Operand to && operator must be convertible to "
                         "logical scalar values."));
            }
        } else {
            Error(_W("Operand to && operator must be convertible to "
                     "logical scalar values."));
        }
    }
    return retval;
}
//=============================================================================
ArrayOf
Evaluator::shortCutAndOperator(AbstractSyntaxTreePtr t)
{
    callstack.pushID((size_t)t->getContext());
    ArrayOf A = expression(t->down);
    ArrayOf B = expression(t->down->right);
    ArrayOf retval = shortCutAndOperator(A, B);
    callstack.popID();
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
