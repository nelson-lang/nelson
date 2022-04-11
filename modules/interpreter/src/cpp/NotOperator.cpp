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
#include "Not.hpp"
#include "OverloadUnaryOperator.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Evaluator::notOperator(AbstractSyntaxTreePtr t)
{
    callstack.pushID((size_t)t->getContext());
    ArrayOf retval = this->notOperator(expression(t->down));
    callstack.popID();
    return retval;
}
//=============================================================================
ArrayOf
Evaluator::notOperator(const ArrayOf& A)
{
    ArrayOf res;
    bool bSuccess = false;
    if ((overloadOnBasicTypes || needToOverloadOperator(A)) && !isOverloadAllowed()) {
        res = OverloadUnaryOperator(this, A, "not", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload;
        res = Not(A, needToOverload);
        if (needToOverload) {
            res = OverloadUnaryOperator(this, A, "not");
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
