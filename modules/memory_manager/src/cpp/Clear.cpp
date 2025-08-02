//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Clear.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "characters_encoding.hpp"
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
                            = getOverloadFunctionName(handleTypeName, "delete");
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
ClearVariable(Evaluator* eval, const std::wstring& variable)
{
    return ClearVariable(eval, wstring_to_utf8(variable));
}
//=============================================================================
bool
ClearVariable(Evaluator* eval, const std::string& variable)
{
    Scope* currentScope = eval->getContext()->getCurrentScope();
    if (!currentScope) {
        return false;
    }
    if (currentScope->getName() == "base") {
        Scope* globalScope = eval->getContext()->getGlobalScope();
        globalScope->deleteVariable(variable);
    }
    callClearHandle(eval, currentScope, variable);
    return currentScope->deleteVariable(variable);
}
//=============================================================================
bool
ClearAllVariables(Evaluator* eval)
{
    bool bUnlocked = true;
    stringVector names;
    eval->getContext()->getCurrentScope()->getVariablesList(true, names);
    for (const auto& name : names) {
        if (ClearVariable(eval, name)) {
            bUnlocked = false;
        }
    }
    return bUnlocked;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
