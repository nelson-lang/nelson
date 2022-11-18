//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "function_handle_extractionBuiltin.hpp"
#include "ArrayOf.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PathFuncManager.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionHandleGateway::function_handle_extractionBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (!argIn.empty()) {
        ArrayOf Arg1 = argIn[0];
        if (Arg1.isFunctionHandle()) {
            function_handle fh = Arg1.getContentAsFunctionHandle();
            FunctionDef* funcDef = nullptr;
            std::wstring functionName;
            if (!fh.name.empty()) {
                eval->getContext()->lookupFunction(fh.name, funcDef);
            } else {
                funcDef = (FunctionDef*)fh.anonymousHandle;
            }
            if (funcDef != nullptr) {
                ArrayOfVector m;
                for (size_t k = 1; k < argIn.size(); k++) {
                    m.push_back(argIn[k]);
                }
                retval = funcDef->evaluateFunction(eval, m, nLhs);
            } else {
                Error(_W("Function does not exist."));
            }
        } else {
            Error(_W("Argument #1 must be a valid function_handle."));
        }
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    return retval;
}
//=============================================================================
