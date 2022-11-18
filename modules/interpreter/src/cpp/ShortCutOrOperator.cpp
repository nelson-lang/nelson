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
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Evaluator::shortCutOrOperator(const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf retval;
    if ((overloadOnBasicTypes || needToOverloadOperator(A) || needToOverloadOperator(B))
        && !isOverloadAllowed()) {
        return OverloadBinaryOperator(this, A, B, "shortcutor");
    }
    if (A.isScalar()) {
        ArrayOf AA(A);
        AA.promoteType(NLS_LOGICAL);
        bool a = AA.getContentAsLogicalScalar() != 0U;
        if (a) {
            return ArrayOf::logicalConstructor(a);
        }
        if (B.isScalar()) {
            ArrayOf BB(B);
            BB.promoteType(NLS_LOGICAL);
            bool b = BB.getContentAsLogicalScalar() != 0U;
            return ArrayOf::logicalConstructor(b);
        }
    }
    Error(_W("Operand to || operator must be convertible to "
             "logical scalar values."));
    return retval;
}
//=============================================================================
ArrayOf
Evaluator::shortCutOrOperator(AbstractSyntaxTreePtr t)
{
    ArrayOf retval;
    callstack.pushID((size_t)t->getContext());
    if (overloadOnBasicTypes && !isOverloadAllowed()) {
        ArrayOf A = expression(t->down);
        ArrayOf B = expression(t->down->right);
        retval = OverloadBinaryOperator(this, A, B, "shortcutand");
        callstack.popID();
        return retval;
    }
    ArrayOf A = expression(t->down);
    if (needToOverloadOperator(A) && isOverloadAllowed()) {
        ArrayOf B = expression(t->down->right);
        retval = OverloadBinaryOperator(this, A, B, "shortcutor");
        callstack.popID();
        return retval;
    }
    if (A.isScalar()) {
        A.promoteType(NLS_LOGICAL);
        bool a = A.getContentAsLogicalScalar() != 0U;
        if (a) {
            retval = ArrayOf::logicalConstructor(a);
            callstack.popID();
            return retval;
        }
        ArrayOf B = expression(t->down->right);
        if (needToOverloadOperator(B) && isOverloadAllowed()) {
            retval = OverloadBinaryOperator(this, A, B, "shortcutor");
            callstack.popID();
            return retval;
        }
        if (B.isScalar()) {
            B.promoteType(NLS_LOGICAL);
            bool b = B.getContentAsLogicalScalar() != 0U;
            retval = ArrayOf::logicalConstructor(b);
            callstack.popID();
            return retval;
        }
    }
    Error(_W("Operand to || operator must be convertible to "
             "logical scalar values."));
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
