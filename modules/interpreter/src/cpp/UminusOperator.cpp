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
#include "Error.hpp"
#include "i18n.hpp"
#include "OverloadHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Evaluator::uminusOperator(const ArrayOf& A)
{
    FunctionDef* funcDef = nullptr;
    if (!FunctionsInMemory::getInstance()->findUnaryOperator(UMINUS_OP, funcDef)) {
        Context* context = this->getContext();
        context->lookupFunction(getOperatorName(UMINUS_OP), funcDef);
        FunctionsInMemory::getInstance()->add(UMINUS_OP, funcDef);
    }
    if (!funcDef) {
        OverloadRequired(getOperatorName(UMINUS_OP));
    }
    ArrayOfVector argsIn;
    argsIn << A;
    ArrayOfVector r = funcDef->evaluateFunction(this, argsIn, 1);
    return r[0];
}
//=============================================================================
}
//=============================================================================
