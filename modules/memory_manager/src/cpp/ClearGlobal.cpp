//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StringHelpers.hpp"
#include "ClearGlobal.hpp"
#include "HandleManager.hpp"
#include "MacroFunctionDef.hpp"
#include "characters_encoding.hpp"
#include "FunctionsInMemory.hpp"
#include "OverloadName.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
callClearHandle(Evaluator* eval, Scope* scope, const std::string& variable)
{
    bool res = false;
    ArrayOf val;
    if (scope->lookupVariable(variable, val)) {
        if (val.isHandle()) {
            Dimensions dimsVal = val.getDimensions();
            auto* qp = (nelson_handle*)val.getDataPointer();
            indexType elementCount = dimsVal.getElementCount();
            for (indexType k = 0; k < elementCount; k++) {
                nelson_handle hl = qp[k];
                HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(hl);
                if (hlObj != nullptr) {
                    std::string handleTypeName = hlObj->getCategory();
                    if (!handleTypeName.empty()) {
                        std::string functionNameClearHandle
                            = getOverloadFunctionName(handleTypeName, "clear");
                        Context* context = eval->getContext();
                        FunctionDef* funcDef = nullptr;
                        if (context->lookupFunction(functionNameClearHandle, funcDef)) {
                            if ((funcDef->type() == NLS_BUILT_IN_FUNCTION)
                                || (funcDef->type() == NLS_MACRO_FUNCTION)) {
                                int nLhs = 0;
                                ArrayOfVector argIn;
                                nelson_handle* ptrObject = static_cast<nelson_handle*>(
                                    ArrayOf::allocateArrayOf(NLS_HANDLE, 1, stringVector(), false));
                                Dimensions dims(1, 1);
                                ptrObject[0] = hl;
                                argIn.push_back(ArrayOf(NLS_HANDLE, dims, (void*)ptrObject));
                                funcDef->evaluateFunction(eval, argIn, nLhs);
                                res = true;
                            }
                        }
                    }
                }
            }
        }
    }
    return res;
}
//=============================================================================
bool
ClearGlobalVariable(Evaluator* eval, const std::wstring& variable)
{
    return ClearGlobalVariable(eval, wstring_to_utf8(variable));
}
//=============================================================================
bool
ClearGlobalVariable(Evaluator* eval, const std::string& variable)
{
    Scope* globalScope = eval->getContext()->getGlobalScope();
    callClearHandle(eval, globalScope, variable);
    bool res = eval->getContext()->getGlobalScope()->deleteVariable(variable);
    return res;
}
//=============================================================================
bool
ClearAllGlobalVariables(Evaluator* eval)
{
    bool bUnlocked = true;
    stringVector names;
    eval->getContext()->getGlobalScope()->getVariablesList(true, names);
    for (const auto& name : names) {
        if (!eval->getContext()->getGlobalScope()->deleteVariable(name)) {
            bUnlocked = false;
        }
    }
    return bUnlocked;
}
//=============================================================================
bool
ClearAllPersistentVariables(Evaluator* eval)
{
    bool bUnlocked = true;
    stringVector names;
    Scope* globalScope = eval->getContext()->getGlobalScope();
    globalScope->getVariablesList(true, names);
    for (const auto& name : names) {
        if (!globalScope->isVariablePersistent(name)) {
            if (!globalScope->deleteVariable(name)) {
                bUnlocked = false;
            }
        }
    }
    return bUnlocked;
}
//=============================================================================
bool
ClearPersistentVariable(Evaluator* eval, const std::wstring& variable)
{
    return ClearPersistentVariable(eval, wstring_to_utf8(variable));
}
//=============================================================================
bool
ClearPersistentVariable(Evaluator* eval, const std::string& variable)
{
    bool res = false;
    FunctionDefPtr func = nullptr;

    if (FunctionsInMemory::getInstance()->find(variable, func)) {
        if (func->type() == NLS_MACRO_FUNCTION) {
            stringVector allVariableNames;
            eval->getContext()->getGlobalScope()->getVariablesList(true, allVariableNames);
            for (const std::string& name : allVariableNames) {
                if (StringHelpers::starts_with(name, "_" + variable + "_")) {
                    res = res || eval->getContext()->getGlobalScope()->deleteVariable(name);
                }
            }
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
