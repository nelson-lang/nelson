//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "fevalBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::fevalBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1);
    Context* context = eval->getContext();
    FunctionDef* funcDef = nullptr;
    ArrayOf param1 = argIn[0];
    if (param1.isFunctionHandle()) {
        function_handle fh = param1.getContentAsFunctionHandle();
        if (fh.anonymousHandle == nullptr) {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_FUNCTION_HANDLE_EXPECTED);
        }
        if (fh.anonymousHandle != nullptr) {
            funcDef = (FunctionDef*)fh.anonymousHandle;
        }
    } else {
        std::string fname = param1.getContentAsCString();
        context->lookupFunction(fname, funcDef);
    }
    if (!funcDef) {
        Error(_W("Invalid anonymous function."));
    }
    ArrayOfVector newarg(argIn);
    newarg.pop_front();
    return funcDef->evaluateFunction(eval, newarg, nLhs);
}
//=============================================================================
