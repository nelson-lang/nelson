//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "subsrefBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::subsrefBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 1);
    if (!argIn[1].isStruct()) {
        Error(L"Nelson:operators:invalidSubsStruct:wrongTypeArgumentNValueExpected",
            L"Wrong type for argument #%d: %s expected.", 2, L"struct");
    }

    Context* context = eval->getContext();
    FunctionDef* funcDef = nullptr;
    std::string fname = "__subsref__";
    if (!context->lookupFunction(fname, funcDef)) {
        Error("Nelson:operators:functionNotAvailable", "function '%s' not available.", fname);
    }
    try {
        eval->withOverload = false;
        retval = funcDef->evaluateFunction(eval, argIn, nLhs);
        eval->withOverload = true;
    } catch (const NelsonException&) {
        eval->withOverload = true;
        throw;
    }
    return retval;
}
//=============================================================================
