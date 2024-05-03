//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "ClassToString.hpp"
#include "ClassName.hpp"
#include "FunctionsInMemory.hpp"
#include "OverloadName.hpp"
#include "NelsonConfiguration.hpp"
#include "HandleGenericObject.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
inline ArrayOfVector
callOverloadedFunctionAllTypes(Evaluator* eval, int nLhs, const ArrayOfVector& argsIn,
    const std::string& functionName, const std::string& commonTypeName, NelsonType commonType,
    bool& wasFound)
{
    wasFound = false;

    if (argsIn.empty() || functionName[0] == OVERLOAD_SYMBOL_CHAR) {
        return {};
    }

    std::string overloadTypeName = getOverloadFunctionName(commonTypeName, functionName);
    FunctionDef* funcDef = nullptr;

    if (!FunctionsInMemory::getInstance()->isNotExistingFunction(overloadTypeName)) {
        if (commonType == NLS_HANDLE && argsIn[0].isHandle() && argsIn[0].isScalar()
            && argsIn[0].isHandleMethod(functionName)) {
            HandleGenericObject* obj = argsIn[0].getContentAsHandleScalar();
            if (obj) {
                wasFound = true;
                return obj->invokeMethod(argsIn, nLhs, functionName);
            }
        }

        eval->getContext()->lookupFunction(overloadTypeName, funcDef);
    }

    if (!funcDef && commonType == NLS_HANDLE) {
        overloadTypeName = getOverloadFunctionName(NLS_HANDLE_STR, functionName);
        eval->getContext()->lookupFunction(overloadTypeName, funcDef);
    }

    if (funcDef) {
        wasFound = true;
        return funcDef->evaluateFunction(eval, argsIn, nLhs);
    }

    return {};
}
//=============================================================================
inline ArrayOfVector
callOverloadedFunction(Evaluator* eval, OverloadLevelCompatibility overloadLevelCompatibility,
    int nLhs, const ArrayOfVector& argsIn, const std::string& functionName,
    const std::string& commonTypeName, NelsonType commonType, bool& wasFound)
{
    switch (overloadLevelCompatibility) {
    case NLS_OVERLOAD_ALL_TYPES: {
        return callOverloadedFunctionAllTypes(
            eval, nLhs, argsIn, functionName, commonTypeName, commonType, wasFound);

    } break;
    case NLS_OVERLOAD_OBJECT_TYPES_ONLY: {
        if (commonType >= NLS_CLASS_ARRAY) {
            return callOverloadedFunctionAllTypes(
                eval, nLhs, argsIn, functionName, commonTypeName, commonType, wasFound);
        }
    } break;
    default:
    case NLS_OVERLOAD_NONE: {
        wasFound = false;
    } break;
    }
    return {};
}
//=============================================================================
inline ArrayOf
callOverloadedFunction(Evaluator* eval, OverloadLevelCompatibility overloadLevelCompatibility,
    const ArrayOfVector& argsIn, const std::string& functionName, const std::string& commonTypeName,
    NelsonType commonType, bool& wasFound, int nargout = 1)
{
    if (!eval->withOverload) {
        wasFound = false;
        return {};
    }
    ArrayOfVector retval = callOverloadedFunction(eval, overloadLevelCompatibility, nargout, argsIn,
        functionName, commonTypeName, commonType, wasFound);
    if (wasFound && nargout > 0) {
        return retval[0];
    }
    return {};
}
//=============================================================================
static bool
OverloadFindFunction(Evaluator* eval, const std::string& forcedFunctionName, FunctionDef** funcDef)
{
    Context* context = eval->getContext();
    return context->lookupFunction(forcedFunctionName, *funcDef);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
