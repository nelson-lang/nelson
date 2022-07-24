//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "func2strBuiltin.hpp"
#include "OverloadFunction.hpp"
#include "Error.hpp"
#include "PathFuncManager.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "AnonymousMacroFunctionDef.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionHandleGateway::func2strBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    ArrayOf arg1 = argIn[0];
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "func2str", bSuccess);
    }
    if (!bSuccess) {
        if (arg1.isFunctionHandle()) {
            function_handle fh = arg1.getContentAsFunctionHandle();
            if (!fh.name.empty()) {
                retval << ArrayOf::characterArrayConstructor(fh.name);
            } else {
                AnonymousMacroFunctionDef* cp = (AnonymousMacroFunctionDef*)fh.anonymousHandle;
                if (cp) {
                    retval << ArrayOf::characterArrayConstructor(cp->getDefinition());
                } else {
                    Error(_W("Invalid anonymous function."));
                }
            }
        } else {
            retval = OverloadFunction(eval, nLhs, argIn, "func2str", bSuccess);
            if (bSuccess) {
                return retval;
            }
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_FUNCTION_HANDLE_EXPECTED);
        }
    }
    return retval;
}
//=============================================================================
