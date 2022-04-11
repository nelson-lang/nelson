//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "castBuiltin.hpp"
#include "Error.hpp"
#include "Cast.hpp"
#include "StringToClass.hpp"
#include "OverloadFunction.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::castBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 1);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "cast", bSuccess);
    }
    if (!bSuccess) {
        bool isSparse = false;
        NelsonType destinationClass;
        if (argIn.size() == 2) {
            ArrayOf param2 = argIn[1];
            std::wstring dest = param2.getContentAsWideString();
            if (eval->isOverloadAllowed()) {
                Context* context = eval->getContext();
                FunctionDef* funcDef = nullptr;
                if (context->lookupFunction(dest, funcDef)) {
                    if ((funcDef->type() == NLS_BUILT_IN_FUNCTION)
                        || (funcDef->type() == NLS_MACRO_FUNCTION)) {
                        bSuccess = true;
                        ArrayOfVector argInCopy;
                        argInCopy.push_back(argIn[0]);
                        return funcDef->evaluateFunction(eval, argInCopy, nLhs);
                    }
                }
            }
            destinationClass = StringToClass(dest);
        } else {
            ArrayOf param2 = argIn[1];
            std::wstring like = param2.getContentAsWideString();
            if (like != L"like") {
                Error(ERROR_WRONG_ARGUMENT_2_VALUE);
            }
            destinationClass = argIn[2].getDataClass();
            isSparse = argIn[2].isSparse();
        }
        retval << Cast(argIn[0], destinationClass, isSparse);
    }
    return retval;
}
//=============================================================================
