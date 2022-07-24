//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "fevalBuiltin.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::fevalBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1);
    Context* context = eval->getContext();
    FunctionDef* funcDef = nullptr;
    std::string fname;
    ArrayOf param1 = argIn[0];
    if (param1.isFunctionHandle()) {
        function_handle fh = param1.getContentAsFunctionHandle();
        if (fh.anonymousHandle == 0 && fh.name.empty()) {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_FUNCTION_HANDLE_EXPECTED);
        }
        if (!fh.name.empty()) {
            fname = fh.name;
            context->lookupFunction(fname, funcDef);
        } else if (fh.anonymousHandle != 0) {
            funcDef = (FunctionDef*)fh.anonymousHandle;
        }
    } else {
        fname = param1.getContentAsCString();
        context->lookupFunction(fname, funcDef);
    }
    if (!funcDef) {
        if (!fname.empty()) {
            Error(_W("function \'") + utf8_to_wstring(fname) + _W("\' is not a function."));
        } else {
            Error(_W("Invalid anonymous function."));
        }
    }
    ArrayOfVector newarg(argIn);
    newarg.pop_front();
    eval->disableOverload();
    ArrayOfVector retval = funcDef->evaluateFunction(eval, newarg, nLhs);
    eval->enableOverload();
    return retval;
}
//=============================================================================
