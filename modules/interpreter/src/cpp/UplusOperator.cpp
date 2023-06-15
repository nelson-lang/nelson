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
#include "OverloadHelpers.hpp"
#include "FunctionsInMemory.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Evaluator::uplusOperator(const ArrayOf& A)
{
    FunctionDef* funcDef = nullptr;
    if (!FunctionsInMemory::getInstance()->findUnaryOperator(UPLUS_OP, funcDef)) {
        Context* context = this->getContext();
        context->lookupFunction(getOperatorName(UPLUS_OP), funcDef);
        FunctionsInMemory::getInstance()->add(UPLUS_OP, funcDef);
    }
    if (!funcDef) {
        OverloadRequired(getOperatorName(UPLUS_OP));
    }
    ArrayOfVector argsIn;
    argsIn << A;
    ArrayOfVector r = funcDef->evaluateFunction(this, argsIn, 1);
    return r[0];
}
//=============================================================================
}
//=============================================================================
