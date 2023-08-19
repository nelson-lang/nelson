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
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
inline std::string
overloadFunctionName(const std::string& destinationTypeName, const std::string& functionName)
{
    return destinationTypeName + "_" + functionName;
}
//=============================================================================
inline ArrayOf
callOverloadedFunctionAllTypes(Evaluator* eval, const ArrayOfVector& argsIn,
    const std::string& functionName, const std::string& commonTypeName, NelsonType commonType,
    bool& wasFound, int nargout = 1)
{
    wasFound = false;
    std::string overloadTypeName = overloadFunctionName(commonTypeName, functionName);
    if (!FunctionsInMemory::getInstance()->isNotExistingFunction(overloadTypeName)) {
        FunctionDef* funcDef = nullptr;
        eval->getContext()->lookupFunction(overloadTypeName, funcDef);
        if (!funcDef && commonType == NLS_HANDLE) {
            overloadTypeName = overloadFunctionName(NLS_HANDLE_STR, functionName);
            eval->getContext()->lookupFunction(overloadTypeName, funcDef);
        }
        if (funcDef) {
            wasFound = true;
            if (nargout == 0) {
                funcDef->evaluateFunction(eval, argsIn, nargout);
                return {};
            }
            return funcDef->evaluateFunction(eval, argsIn, nargout)[0];
        }
    } else if (commonType == NLS_HANDLE) {
        FunctionDef* funcDef = nullptr;
        overloadTypeName = overloadFunctionName(NLS_HANDLE_STR, functionName);
        eval->getContext()->lookupFunction(overloadTypeName, funcDef);
        if (funcDef) {
            wasFound = true;
            if (nargout == 0) {
                funcDef->evaluateFunction(eval, argsIn, nargout);
                return {};
            }
            return funcDef->evaluateFunction(eval, argsIn, nargout)[0];
        }
    }
    return {};
}
//=============================================================================
inline ArrayOf
callOverloadedFunction(Evaluator* eval, OverloadLevelCompatibility overloadLevelCompatibility,
    const ArrayOfVector& argsIn, const std::string& functionName, const std::string& commonTypeName,
    NelsonType commonType, bool& wasFound, int nargout = 1)
{
    switch (overloadLevelCompatibility) {
    case NLS_OVERLOAD_ALL_TYPES: {
        return callOverloadedFunctionAllTypes(
            eval, argsIn, functionName, commonTypeName, commonType, wasFound, nargout);

    } break;
    case NLS_OVERLOAD_OBJECT_TYPES_ONLY: {
        if (commonType >= NLS_CLASS_ARRAY) {
            return callOverloadedFunctionAllTypes(
                eval, argsIn, functionName, commonTypeName, commonType, wasFound, nargout);
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
inline void
OverloadRequired(const std::string& functionName)
{
    std::string msgfmt
        = _("Check for incorrect argument data type or missing argument in call to function '%s'.");
    size_t size = msgfmt.size() + functionName.size();
    std::string msg(size, '\0');
    int nChars = std::snprintf(&msg[0], size, msgfmt.c_str(), functionName.c_str());
    msg.resize(nChars);
    Error(msg, "Nelson:UndefinedFunction");
}
//=============================================================================
static bool
OverloadFindFunction(Evaluator* eval, const std::string& forcedFunctionName, FunctionDef** funcDef)
{
    Context* context = eval->getContext();
    return context->lookupFunction(forcedFunctionName, *funcDef);
}
//=============================================================================
static ArrayOf
callOverloadedFunction(Evaluator* eval, const ArrayOfVector& argsIn,
    const std::string& OverloadNameDesired, bool wasFound, FunctionDef* funcDef, bool bRaiseError)
{
    ArrayOf res;
    if (!wasFound) {
        if (bRaiseError) {
            Error(std::string("function ") + OverloadNameDesired + " undefined.");
        } else {
            res = ArrayOf::emptyConstructor();
        }
    } else {
        int nargout = 1;
        ArrayOfVector val = funcDef->evaluateFunction(eval, argsIn, nargout);
        if (val.size() != 1) {
            if (bRaiseError) {
                Error(std::string("function ") + funcDef->getName()
                    + " only one output argument expected.");
            }
            return ArrayOf::emptyConstructor();
        }
        res = val[0];
    }
    return res;
}
//=============================================================================

} // namespace Nelson
//=============================================================================
