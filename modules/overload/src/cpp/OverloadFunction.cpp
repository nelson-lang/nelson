//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "OverloadFunction.hpp"
#include "ClassName.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
namespace Nelson {
ArrayOfVector
OverloadFunction(Evaluator* eval, int nLhs, const ArrayOfVector& argIn,
    const std::string& functionName, bool& bSuccess)
{
    if (!functionName.empty()) {
        if (eval->isOverloadAllowed()) {
            Context* context = eval->getContext();
            FunctionDef* funcDef = nullptr;
            std::string OverloadName = ClassName(argIn[0]) + "_" + functionName;
            if (context->lookupFunction(OverloadName, funcDef)) {
                if ((funcDef->type() == NLS_BUILT_IN_FUNCTION)
                    || (funcDef->type() == NLS_MACRO_FUNCTION)) {
                    bSuccess = true;
                    ArrayOfVector argInCopy = const_cast<ArrayOfVector&>(argIn);
                    return funcDef->evaluateFunction(eval, argInCopy, nLhs);
                }
            }
        }
    }
    bSuccess = false;
    return {};
}
//=============================================================================
ArrayOfVector
OverloadFunction(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn, const std::string& functionName)
{
    bool bSuccess;
    ArrayOfVector res = OverloadFunction(eval, nLhs, argIn, functionName, bSuccess);
    if (!bSuccess) {
        OverloadRequired(eval, argIn, Overload::OverloadClass::FUNCTION, functionName);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
