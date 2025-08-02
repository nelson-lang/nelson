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
#include "Error.hpp"
#include "i18n.hpp"
#include "Operators.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
shortCutOrOperatorImpl(Evaluator* eval, AbstractSyntaxTreePtr t);
//=============================================================================
ArrayOf
Evaluator::shortCutOrOperator(AbstractSyntaxTreePtr t)
{
    callstack.pushID((size_t)t->getContext());
    ArrayOf res = shortCutOrOperatorImpl(this, t);
    callstack.popID();
    return res;
}
//=============================================================================
ArrayOf
shortCutOrOperatorImpl(Evaluator* eval, AbstractSyntaxTreePtr t)
{
    ArrayOf A = eval->expression(t->down);
    if (!A.isScalar()) {
        Error(_W("Operand to || operator must be convertible to logical scalar values."));
    }
    try {
        A.promoteType(NLS_LOGICAL);
    } catch (const Exception&) {
        Error(_W("Operand to || operator must be convertible to logical scalar values."));
    }
    bool a = A.getContentAsLogicalScalar() != 0U;
    if (a) {
        return ArrayOf::logicalConstructor(a);
    }
    ArrayOf B = eval->expression(t->down->right);
    if (!B.isScalar()) {
        Error(_W("Operand to || operator must be convertible to logical scalar values."));
    }
    try {
        B.promoteType(NLS_LOGICAL);
    } catch (const Exception&) {
        Error(_W("Operand to || operator must be convertible to logical scalar values."));
    }
    return ArrayOf::logicalConstructor(B.getContentAsLogicalScalar() != 0U);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
