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
#include "UnaryPlus.hpp"
#include "OverloadHelpers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Evaluator::uplusOperator(AbstractSyntaxTreePtr t)
{
    callstack.pushID((size_t)t->getContext());
    ArrayOf retval = this->uplusOperator(expression(t->down));
    callstack.popID();
    return retval;
}
//=============================================================================
ArrayOf
Evaluator::uplusOperator(const ArrayOf& A)
{
    bool wasFound = false;
    ArrayOf res = callOverloadedFunction(this,
        NelsonConfiguration::getInstance()->getOverloadLevelCompatibility(), A, UPLUS_OPERATOR_STR,
        ClassName(A), A.getDataClass(), wasFound);
    if (wasFound) {
        return res;
    }
    bool needToOverload = false;
    res = UnaryPlus(A, needToOverload);
    if (needToOverload) {
        OverloadRequired(UPLUS_OPERATOR_STR);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
