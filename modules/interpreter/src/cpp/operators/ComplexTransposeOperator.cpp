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
#include "OverloadHelpers.hpp"
#include "OverloadRequired.hpp"
#include "ComplexTranspose.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Evaluator::complexTransposeOperator(AbstractSyntaxTreePtr t)
{
    callstack.pushID((size_t)t->getContext());
    ArrayOf retval = this->complexTransposeOperator(expression(t->down));
    callstack.popID();
    return retval;
}
//=============================================================================
ArrayOf
Evaluator::complexTransposeOperator(const ArrayOf& A)
{
    bool wasFound = false;
    ArrayOf res = callOverloadedFunction(this,
        NelsonConfiguration::getInstance()->getOverloadLevelCompatibility(), A,
        CTRANSPOSE_OPERATOR_STR, ClassName(A), A.getDataClass(), wasFound);
    if (wasFound) {
        return res;
    }
    bool needToOverload = false;
    res = ComplexTranspose(A, needToOverload);
    if (needToOverload) {
        OverloadRequired(CTRANSPOSE_OPERATOR_STR);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
