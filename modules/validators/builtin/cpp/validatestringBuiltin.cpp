//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "validatestringBuiltin.hpp"
#include "Evaluator.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ValidatorsGateway::validatestringBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2, 5);
    Evaluator* eval = (Evaluator*)NelsonConfiguration::getInstance()->getMainEvaluator();
    FunctionDef* fptr = nullptr;
    if (!eval->getContext()->lookupFunction("__validatestring__", fptr)) {
        Error(L"Nelson:validators:helperNotFound", L"Validation helper not found.");
    }
    return fptr->evaluateFunction(eval, argIn, 1);
}
//=============================================================================
