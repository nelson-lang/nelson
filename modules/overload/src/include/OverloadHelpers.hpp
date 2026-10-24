//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
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
#include "Operators.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
inline bool
isOverloadObjectType(NelsonType commonType)
{
    return commonType >= NLS_CLASS_ARRAY && commonType < NLS_UNKNOWN;
}
//=============================================================================
inline bool
hasNativeHandleFallback(NelsonType commonType)
{
    return commonType >= NLS_HANDLE && commonType < NLS_UNKNOWN;
}
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

    // A native handle method (e.g. a Python cast such as double()) is invoked through the
    // object and does not need an overload function `@type/name`. Do not gate it behind the
    // not-existing-function cache: a failed probe such as `obj.double()` records that name as
    // missing and would suppress the handle method on later calls.
    if (commonType == NLS_HANDLE && argsIn[0].isHandle() && argsIn[0].isScalar()
        && argsIn[0].isHandleMethod(functionName)) {
        HandleGenericObject* obj = argsIn[0].asHandleScalar();
        if (obj) {
            ArrayOfVector results;
            Interface* objInterface = nullptr;
            if (eval) {
                objInterface = eval->getInterface();
            }
            if (obj->invokeMethod(objInterface, argsIn, nLhs, functionName, results)) {
                wasFound = true;
                return results;
            }
        }
    }

    if (!FunctionsInMemory::getInstance()->isNotExistingFunction(overloadTypeName)) {
        lookupOverloadFunctionCached(eval->getContext(), overloadTypeName, funcDef);
    }

    if (!funcDef && hasNativeHandleFallback(commonType)) {
        const std::string fallbackTypeName = ClassToString(commonType);
        if (!fallbackTypeName.empty() && fallbackTypeName != commonTypeName) {
            overloadTypeName = getOverloadFunctionName(fallbackTypeName, functionName);
            lookupOverloadFunctionCached(eval->getContext(), overloadTypeName, funcDef);
        }
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
        if (isOverloadObjectType(commonType)) {
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
// sprintf/fprintf read arguments as raw numeric/char data and have no overload dispatch.
// Class and handle arguments (all but the format, argument 0) are converted to char() first,
// so an object that defines char() prints as its text. Objects without a char overload are
// left untouched.
inline ArrayOfVector
convertObjectPrintfArguments(Evaluator* eval, const ArrayOfVector& argsIn)
{
    if (!eval || !eval->withOverload) {
        return argsIn;
    }
    ArrayOfVector result;
    bool converted = false;
    for (size_t k = 0; k < argsIn.size(); ++k) {
        ArrayOf value = argsIn[k];
        if (k > 0 && (value.isClassType() || value.isHandle())) {
            bool wasFound = false;
            ArrayOf asChar = callOverloadedFunction(eval,
                NelsonConfiguration::getInstance()->getOverloadLevelCompatibility(), { value },
                "char", ClassName(value), value.dataClass(), wasFound);
            if (wasFound) {
                value = asChar;
                converted = true;
            }
        }
        result.push_back(value);
    }
    return converted ? result : argsIn;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
