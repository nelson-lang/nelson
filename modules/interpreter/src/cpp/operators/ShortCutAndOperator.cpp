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
#include "OverloadHelpers.hpp"
#include "OverloadRequired.hpp"
#include "FindCommonType.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "Operators.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
shortCutAndOperatorImpl(Evaluator* eval, AbstractSyntaxTreePtr t);
//=============================================================================
ArrayOf
Evaluator::shortCutAndOperator(AbstractSyntaxTreePtr t)
{
    callstack.pushID((size_t)t->getContext());
    ArrayOf res = shortCutAndOperatorImpl(this, t);
    callstack.popID();
    return res;
}
//=============================================================================
ArrayOf
shortCutAndOperatorImpl(Evaluator* eval, AbstractSyntaxTreePtr t)
{
    ArrayOf A = eval->expression(t->down);
    if (!A.isScalar()) {
        raiseError(L"Nelson:interpreter:ERROR_OPERAND_TO_OPERATOR_MUST_BE_CONVERTIBLE_TO_LOGICAL_"
                   L"SCALAR_VALUES",
            ERROR_OPERAND_TO_OPERATOR_MUST_BE_CONVERTIBLE_TO_LOGICAL_SCALAR_VALUES);
    }
    if (A.isComplex()) {
        raiseError(L"Nelson:interpreter:ERROR_COMPLEX_VALUES_CANNOT_CONVERTED_TO_LOGICALS",
            ERROR_COMPLEX_VALUES_CANNOT_CONVERTED_TO_LOGICALS);
    }
    try {
        A.promoteType(NLS_LOGICAL);
    } catch (const Exception&) {
        raiseError(L"Nelson:interpreter:ERROR_OPERAND_TO_OPERATOR_MUST_BE_CONVERTIBLE_TO_LOGICAL_"
                   L"SCALAR_VALUES",
            ERROR_OPERAND_TO_OPERATOR_MUST_BE_CONVERTIBLE_TO_LOGICAL_SCALAR_VALUES);
    }
    bool a = A.getContentAsLogicalScalar() != 0U;
    if (!a) {
        return ArrayOf::logicalConstructor(a);
    }
    ArrayOf B = eval->expression(t->down->right);
    if (!B.isScalar()) {
        raiseError(L"Nelson:interpreter:ERROR_OPERAND_TO_OPERATOR_MUST_BE_CONVERTIBLE_TO_LOGICAL_"
                   L"SCALAR_VALUES",
            ERROR_OPERAND_TO_OPERATOR_MUST_BE_CONVERTIBLE_TO_LOGICAL_SCALAR_VALUES);
    }
    if (B.isComplex()) {
        raiseError(L"Nelson:interpreter:ERROR_COMPLEX_VALUES_CANNOT_CONVERTED_TO_LOGICALS",
            ERROR_COMPLEX_VALUES_CANNOT_CONVERTED_TO_LOGICALS);
    }
    try {
        B.promoteType(NLS_LOGICAL);
    } catch (const Exception&) {
        raiseError(L"Nelson:interpreter:ERROR_OPERAND_TO_OPERATOR_MUST_BE_CONVERTIBLE_TO_LOGICAL_"
                   L"SCALAR_VALUES",
            ERROR_OPERAND_TO_OPERATOR_MUST_BE_CONVERTIBLE_TO_LOGICAL_SCALAR_VALUES);
    }
    return ArrayOf::logicalConstructor(B.getContentAsLogicalScalar() != 0U);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
